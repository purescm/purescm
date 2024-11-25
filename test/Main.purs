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
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..), either, isRight)
import Data.Foldable as Foldable
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Set as Set
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.TraversableWithIndex (forWithIndex)
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Glob.Basic (expandGlobs)
import Node.Library.Execa.Which (defaultWhichOptions, which)
import Node.Path as Path
import Node.Process as Process
import Partial.Unsafe (unsafeCrashWith)
import PureScript.Backend.Chez.Constants (moduleLib, schemeExt)
import Test.Utils (canRunMain, execWithStdin, loadModuleMain, mkdirp, spawnFromParent)

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
  currentDirectory <- liftEffect Process.cwd
  let runtimeLibPath = Path.concat [ currentDirectory, "lib" ]

  -- Get into the snapshot project and build it
  liftEffect $ Process.chdir $ Path.concat [ "test-snapshots" ]
  spawnFromParent "spago" [ "build" ]
  snapshotDir <- liftEffect Process.cwd
  let snapshotInput = Path.concat [ snapshotDir, "src", "snapshots-input" ]
  let snapshotsOut = Path.concat [ snapshotDir, "src", "snapshots-output" ]
  snapshotPaths <- expandGlobs snapshotInput (map (_ <> ".purs") $ NonEmptyArray.toArray filter)
  mkdirp snapshotsOut
  schemeBin <- getSchemeBinary

  -- Tests return true or false depending on whether they pass
  results <- forWithIndex (Set.toUnfoldable snapshotPaths :: Array _) \i snapshotInputPath -> do
    let
      -- The idea here is that we get files from the `output` directory, and check if
      -- they look like the ones we have in `snapshots-output`
      expectedSnapshotOutputPath =
        String.replace (Pattern snapshotInput) (Replacement snapshotsOut)
          $ String.replace (Pattern ".purs") (Replacement schemeExt)
          $ snapshotInputPath
      actualSnapshotOutputPath =
        String.replace (Pattern snapshotInput) (Replacement (Path.concat [ snapshotDir, "output" ]))
          $ String.replace (Pattern ".purs") (Replacement $ "/" <> moduleLib <> schemeExt)
          $ snapshotInputPath
      name = String.replace (Pattern $ snapshotInput <> "/") (Replacement "")
        $ String.replace (Pattern ".purs") (Replacement "")
        $ snapshotInputPath

    let
      total = show $ Set.size snapshotPaths
      index = show i
      padding = power " " (SCU.length total - SCU.length index)
    Console.log $ "[" <> padding <> index <> " of " <> total <> "] Running test " <> name

    -- Read the file generated by purescm
    actualSnapshotOutput <- FS.readTextFile UTF8 actualSnapshotOutputPath
    purescriptFile <- FS.readTextFile UTF8 snapshotInputPath
    hasMain <- either unsafeCrashWith pure $ canRunMain purescriptFile

    let
      runAcceptedTest = do
        result <- loadModuleMain
          { libdir: runtimeLibPath <> ":output"
          , scheme: schemeBin
          , hasMain
          , modulePath: actualSnapshotOutputPath
          , moduleName: name
          }
        let shouldFail = isFailingTestFile purescriptFile
        case result.exitCode of
          Just 0 -> pure true
          Just 0 | shouldFail -> do
            Console.log $ withGraphics (foreground Red) "✗" <> " " <> name <> " should have failed."
            Console.log result.message
            pure false
          _ | shouldFail -> pure true
          _ -> do
            Console.log $ withGraphics (foreground Red) "✗" <> " " <> name <> " failed."
            Console.log result.message
            pure false

    attempt (FS.readTextFile UTF8 expectedSnapshotOutputPath) >>= case _ of
      -- If we don't have this fixture yet, save it
      Left _ -> do
        Console.log $ withGraphics (foreground Yellow) "✓" <> " " <> name <> " saved."
        FS.writeTextFile UTF8 expectedSnapshotOutputPath actualSnapshotOutput
        pure true
      Right prevOutput
        -- We have and it looks the same  -> run it
        | actualSnapshotOutput == prevOutput ->
            runAcceptedTest
        -- We have it but we are accepting the new output -> save it, and run it afterwards
        | accept -> do
            Console.log $ withGraphics (foreground Yellow) "✓" <> " " <> name <> " accepted."
            FS.writeTextFile UTF8 expectedSnapshotOutputPath actualSnapshotOutput
            runAcceptedTest
        -- We have it and it's broken
        | otherwise -> do
            Console.log $ withGraphics (foreground Red) "✗" <> " " <> name <> " failed."
            diff <- _.stdout <$> execWithStdin "diff" [ expectedSnapshotOutputPath, "-" ]
              actualSnapshotOutput
            Console.log diff
            pure false

  unless (Foldable.and results) do
    liftEffect $ Process.exit' 1

isFailingTestFile :: String -> Boolean
isFailingTestFile = String.contains (Pattern "Failing")

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
