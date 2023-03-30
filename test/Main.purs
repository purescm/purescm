-- A majority of the code below was copied from 
-- https://github.com/aristanetworks/purescript-backend-optimizer/blob/main/backend-es/test/Main.purs
-- To fullfill copyright requirements...
--    Copyright © 2022 Arista Networks, Inc.
--    MIT license: https://opensource.org/license/mit/
module Test.Main where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)
import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Data.Array (findMap)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..), either, isRight)
import Data.Foldable (for_)
import Data.Foldable as Foldable
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid (power)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..))
import Dodo (plainText)
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Glob.Basic (expandGlobs)
import Node.Library.Execa.Which (defaultWhichOptions, which)
import Node.Path as Path
import Node.Process as Process
import Partial.Unsafe (unsafeCrashWith)
import PureScript.Backend.Chez.Constants (moduleForeign, moduleLib, schemeExt)
import PureScript.Backend.Chez.Convert (codegenModule)
import PureScript.Backend.Chez.Runtime (runtimeModule)
import PureScript.Backend.Chez.Syntax as S
import PureScript.Backend.Optimizer.Builder (buildModules)
import PureScript.Backend.Optimizer.Convert (BackendModule)
import PureScript.Backend.Optimizer.CoreFn (Comment(..), Module(..), ModuleName(..))
import PureScript.Backend.Optimizer.Directives (parseDirectiveFile)
import PureScript.Backend.Optimizer.Directives.Defaults (defaultDirectives)
import PureScript.Backend.Optimizer.Semantics.Foreign (coreForeignSemantics)
import Test.Utils (bufferToUTF8, canRunMain, copyFile, coreFnModulesFromOutput, execWithStdin, loadModuleMain, mkdirp, spawnFromParent)

type TestArgs =
  { accept :: Boolean
  , filter :: NonEmptyArray String
  }

argParser :: ArgParser TestArgs
argParser =
  ArgParser.fromRecord
    { accept:
        ArgParser.flag [ "--accept", "-a" ]
          "Accepts snapshot output"
          # ArgParser.boolean
          # ArgParser.default false
    , filter:
        ArgParser.argument [ "--filter", "-f" ]
          "Filter tests matching a prefix"
          # ArgParser.unfolded1
          # ArgParser.default (pure "Snapshot.*")
    }

main :: Effect Unit
main = do
  cliArgs <- Array.drop 2 <$> Process.argv
  case ArgParser.parseArgs "test" "" argParser cliArgs of
    Left err ->
      Console.error $ ArgParser.printArgError err
    Right args ->
      launchAff_ $ runSnapshotTests args

runSnapshotTests :: TestArgs -> Aff Unit
runSnapshotTests { accept, filter } = do
  liftEffect $ Process.chdir $ Path.concat [ "test-snapshots" ]
  spawnFromParent "spago" [ "build", "--purs-args", "-g corefn" ]
  snapshotDir <- liftEffect Process.cwd
  snapshotPaths <- expandGlobs (Path.concat [ snapshotDir, "src", "snapshots-input" ])
    [ "Snapshot.*.purs" ]
  schemeBin <- getSchemeBinary
  outputRef <- liftEffect $ Ref.new Map.empty
  let snapshotsOut = Path.concat [ snapshotDir, "src", "snapshots-output" ]
  let testOut = Path.concat [ snapshotDir, "test-out" ]
  mkdirp snapshotsOut
  mkdirp testOut
  -- RUNTIME
  let runtimePath = Path.concat [ testOut, "_Chez_Runtime" ]
  mkdirp runtimePath
  let runtimeFilePath = Path.concat [ runtimePath, moduleLib <> schemeExt ]
  let runtimeContents = Dodo.print plainText Dodo.twoSpaces $ S.printLibrary $ runtimeModule
  FS.writeTextFile UTF8 runtimeFilePath runtimeContents
  coreFnModulesFromOutput "output" filter >>= case _ of
    Left errors -> do
      for_ errors \(Tuple filePath err) -> do
        Console.error $ filePath <> " " <> err
      liftEffect $ Process.exit 1
    Right coreFnModules -> do
      let { directives } = parseDirectiveFile defaultDirectives
      -- No runtime .ss files needed yet
      -- copyFile (Path.concat [ "..", "..", "runtime.js" ]) (Path.concat [ testOut, "runtime.js" ])
      coreFnModules # buildModules
        { directives
        , foreignSemantics: coreForeignSemantics -- no chez scheme specific foreign semantics yet
        , onCodegenModule: \_ (Module { name: ModuleName name, path }) backend -> do
            let
              formatted =
                Dodo.print plainText Dodo.twoSpaces
                  $ S.printLibrary
                  $ codegenModule backend
            let testFileDir = Path.concat [ testOut, name ]
            let testFilePath = Path.concat [ testFileDir, moduleLib <> schemeExt ]
            mkdirp testFileDir
            FS.writeTextFile UTF8 testFilePath formatted
            unless (Set.isEmpty backend.foreign) do
              let
                foreignSiblingPath =
                  fromMaybe path (String.stripSuffix (Pattern (Path.extname path)) path) <>
                    schemeExt
              let foreignOutputPath = Path.concat [ testFileDir, moduleForeign <> schemeExt ]
              copyFile foreignSiblingPath foreignOutputPath
            let snapshotDirFile = Path.concat [ snapshotDir, path ]
            when (Set.member snapshotDirFile snapshotPaths) do
              originalFileSourceCode <- FS.readTextFile UTF8 snapshotDirFile
              hasMain <- either unsafeCrashWith pure $
                canRunMain originalFileSourceCode
              void $ liftEffect $ Ref.modify
                (Map.insert name ({ formatted, failsWith: hasFails backend, hasMain }))
                outputRef
        , onPrepareModule: \build coreFnMod@(Module { name }) -> do
            let total = show build.moduleCount
            let index = show (build.moduleIndex + 1)
            let padding = power " " (SCU.length total - SCU.length index)
            Console.log $ "[" <> padding <> index <> " of " <> total <> "] Building " <> unwrap name
            pure coreFnMod
        }
      outputModules <- liftEffect $ Ref.read outputRef
      results <- forWithIndex outputModules \name ({ formatted, failsWith, hasMain }) -> do
        let
          snapshotFilePath = Path.concat [ snapshotsOut, name <> schemeExt ]
          runAcceptedTest = do
            schemeFile <- liftEffect $ Path.resolve [ testOut, name ] $ moduleLib <> schemeExt
            result <- loadModuleMain testOut schemeBin hasMain schemeFile
            case result of
              Left err | matchesFail err.message failsWith -> do
                Console.log $ withGraphics (foreground Red) "✗" <> " " <> name <> " failed."
                Console.log err.message
                pure false
              Right _ | isJust failsWith -> do
                Console.log $ withGraphics (foreground Red) "✗" <> " " <> name <>
                  " succeeded when it should have failed."
                pure false
              _ ->
                pure true
        attempt (FS.readTextFile UTF8 snapshotFilePath) >>= case _ of
          Left _ -> do
            Console.log $ withGraphics (foreground Yellow) "✓" <> " " <> name <> " saved."
            FS.writeTextFile UTF8 snapshotFilePath formatted
            pure true
          Right prevOutput
            | formatted == prevOutput ->
                runAcceptedTest
            | accept -> do
                Console.log $ withGraphics (foreground Yellow) "✓" <> " " <> name <> " accepted."
                FS.writeTextFile UTF8 snapshotFilePath formatted
                runAcceptedTest
            | otherwise -> do
                Console.log $ withGraphics (foreground Red) "✗" <> " " <> name <> " failed."
                diff <- bufferToUTF8 <<< _.stdout =<< execWithStdin
                  ("diff " <> snapshotFilePath <> " -")
                  formatted
                Console.log diff
                pure false
      unless (Foldable.and results) do
        liftEffect $ Process.exit 1

hasFails :: BackendModule -> Maybe String
hasFails = findMap go <<< _.comments
  where
  go = case _ of
    LineComment comm ->
      String.stripPrefix (Pattern "@fails ") (String.trim comm)
    _ ->
      Nothing

matchesFail :: String -> Maybe String -> Boolean
matchesFail errMsg = case _ of
  Just msg ->
    not $ String.contains (Pattern msg) errMsg
  Nothing ->
    true

getSchemeBinary :: Aff String
getSchemeBinary = do
  useScheme <- isRight <$> which "scheme" defaultWhichOptions
  if useScheme then do
    pure "scheme"
  else do
    useChez <- isRight <$> which "chez" defaultWhichOptions
    if useChez then
      pure "chez"
    else do
      unsafeCrashWith "Could not find scheme binary"
