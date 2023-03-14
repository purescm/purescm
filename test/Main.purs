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
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Foldable as Foldable
import Data.Map as Map
import Data.Maybe (Maybe(..))
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
import Effect.Aff (Aff, Error, attempt, launchAff_)
import Effect.Aff as Error
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Glob.Basic (expandGlobs)
import Node.Path as Path
import Node.Process as Process
import PureScript.Backend.Chez.Convert (codegenModule)
import PureScript.Backend.Chez.Syntax as S
import PureScript.Backend.Optimizer.Builder (buildModules)
import PureScript.Backend.Optimizer.Convert (BackendModule)
import PureScript.Backend.Optimizer.CoreFn (Comment(..), Module(..), ModuleName(..))
import PureScript.Backend.Optimizer.Directives (parseDirectiveFile)
import PureScript.Backend.Optimizer.Directives.Defaults (defaultDirectives)
import PureScript.Backend.Optimizer.Semantics.Foreign (coreForeignSemantics)
import Test.Utils (bufferToUTF8, coreFnModulesFromOutput, execWithStdin, mkdirp, spawnFromParent)

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
  spawnFromParent "spago" [ "build", "-u", "-g corefn" ]
  snapshotDir <- liftEffect Process.cwd
  snapshotPaths <- expandGlobs (Path.concat [ snapshotDir, "snapshots-input" ])
    [ "Snapshot.*.purs" ]
  outputRef <- liftEffect $ Ref.new Map.empty
  let snapshotsOut = Path.concat [ snapshotDir, "snapshots-output" ]
  let testOut = Path.concat [ snapshotDir, "test-out" ]
  mkdirp snapshotsOut
  mkdirp testOut
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
                  $ flip append Dodo.break
                  $ S.printChezExpr
                  $ codegenModule backend
            let testFileDir = Path.concat [ testOut, name ]
            let testFilePath = Path.concat [ testFileDir, "output.ss" ]
            mkdirp testFileDir
            FS.writeTextFile UTF8 testFilePath formatted
            -- Not worried about FFI yet
            -- unless (Set.isEmpty backend.foreign) do
            --   let
            --     foreignSiblingPath =
            --       fromMaybe path (String.stripSuffix (Pattern (Path.extname path)) path) <> ".ss"
            --   let foreignOutputPath = Path.concat [ testFileDir, "foreign.ss" ]
            --   copyFile foreignSiblingPath foreignOutputPath
            when (Set.member (Path.concat [ snapshotDir, path ]) snapshotPaths) do
              void $ liftEffect $ Ref.modify (Map.insert name (Tuple formatted (hasFails backend)))
                outputRef
        , onPrepareModule: \build coreFnMod@(Module { name }) -> do
            let total = show build.moduleCount
            let index = show (build.moduleIndex + 1)
            let padding = power " " (SCU.length total - SCU.length index)
            Console.log $ "[" <> padding <> index <> " of " <> total <> "] Building " <> unwrap name
            pure coreFnMod
        }
      outputModules <- liftEffect $ Ref.read outputRef
      results <- forWithIndex outputModules \name (Tuple output _failsWith) -> do
        let
          snapshotFilePath = Path.concat [ snapshotsOut, name <> ".ss" ]
        -- Not doing acceptance tests yet
        -- runAcceptedTest = do
        --   result <- attempt $ foldMap liftEffect =<< loadModuleMain =<< liftEffect
        --     (Path.resolve [ testOut, name ] "output.ss")
        --   case result of
        --     Left err | matchesFail err failsWith -> do
        --       Console.log $ withGraphics (foreground Red) "✗" <> " " <> name <> " failed."
        --       Console.log $ Error.message err
        --       pure false
        --     Right _ | isJust failsWith -> do
        --       Console.log $ withGraphics (foreground Red) "✗" <> " " <> name <>
        --         " succeeded when it should have failed."
        --       pure false
        --     _ ->
        --       pure true
        attempt (FS.readTextFile UTF8 snapshotFilePath) >>= case _ of
          Left _ -> do
            Console.log $ withGraphics (foreground Yellow) "✓" <> " " <> name <> " saved."
            FS.writeTextFile UTF8 snapshotFilePath output
            pure true
          Right prevOutput
            | output == prevOutput ->
                -- runAcceptedTest
                pure true
            | accept -> do
                Console.log $ withGraphics (foreground Yellow) "✓" <> " " <> name <> " accepted."
                FS.writeTextFile UTF8 snapshotFilePath output
                -- runAcceptedTest
                pure true
            | otherwise -> do
                Console.log $ withGraphics (foreground Red) "✗" <> " " <> name <> " failed."
                diff <- bufferToUTF8 <<< _.stdout =<< execWithStdin
                  ("diff " <> snapshotFilePath <> " -")
                  output
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

matchesFail :: Error -> Maybe String -> Boolean
matchesFail err = case _ of
  Just msg ->
    not $ String.contains (Pattern msg) $ Error.message err
  Nothing ->
    true
