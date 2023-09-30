module Main where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Data.Array as Array
import Data.Either (Either(..), isRight)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (power)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Dodo (plainText)
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, attempt, effectCanceler, error, launchAff_, makeAff, runAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (message)
import Node.Encoding (Encoding(..))
import Node.EventEmitter (on_)
import Node.FS.Aff as FS
import Node.FS.Perms (mkPerms)
import Node.FS.Perms as Perms
import Node.FS.Stats as Stats
import Node.FS.Stream (createReadStream, createWriteStream)
import Node.Library.Execa (ExecaResult, execa)
import Node.Library.Execa.Which (defaultWhichOptions, which)
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import Node.Stream as Stream
import Partial.Unsafe (unsafeCrashWith)
import PureScript.Backend.Chez.Builder (basicBuildMain)
import PureScript.Backend.Chez.Constants (moduleForeign, moduleLib, schemeExt)
import PureScript.Backend.Chez.Convert (codegenModule)
import PureScript.Backend.Chez.Printer as Printer
import PureScript.Backend.Chez.Runtime (runtimeModule)
import PureScript.Backend.Optimizer.CoreFn (Module(..), ModuleName(..))
import Spago.Generated.BuildInfo as BuildInfo

type BuildArgs =
  { coreFnDir :: FilePath
  , outputDir :: FilePath
  , directivesFile :: Maybe FilePath
  }

type BundleArgs =
  { moduleName :: String
  , libDir :: FilePath
  , outputDir :: FilePath
  }

type RunArgs =
  { moduleName :: String
  , libDir :: FilePath
  }

data Command
  = Build BuildArgs
  | Bundle BundleArgs
  | Run RunArgs

cliArgParser :: ArgParser Command
cliArgParser =
  ArgParser.choose "command"
    [ ArgParser.command [ "build" ]
        "Builds Chez scheme code from corefn.json files"
        do
          Build <$> buildCmdArgParser <* ArgParser.flagHelp
    , ArgParser.command [ "bundle-app" ]
        "Bundles .so files to a single program file."
        do Bundle <$> bundleCmdArgParser <* ArgParser.flagHelp
    , ArgParser.command [ "run" ]
        "Runs a compiled scheme program by invoking the main function using the Chez interpreter."
        do Run <$> runCmdArgParser <* ArgParser.flagHelp
    ]
    <* ArgParser.flagHelp
    <* ArgParser.flagInfo [ "--version", "-v" ] "Show the current version of purescm."
      BuildInfo.buildInfo.packages.purescm

buildCmdArgParser :: ArgParser BuildArgs
buildCmdArgParser =
  ArgParser.fromRecord
    { coreFnDir:
        ArgParser.argument [ "--corefn-dir" ]
          "Path to input directory containing corefn.json files.\n\
          \Defaults to './output'."
          # ArgParser.default (Path.concat [ ".", "output" ])
    , outputDir:
        ArgParser.argument [ "--output-dir" ]
          "Path to output directory for backend files.\n\
          \Defaults to './output'."
          # ArgParser.default (Path.concat [ ".", "output" ])
    , directivesFile:
        ArgParser.argument [ "--directives" ]
          "Path to file that defines external inline directives (optional)."
          # ArgParser.optional
    }

bundleCmdArgParser :: ArgParser BundleArgs
bundleCmdArgParser =
  ArgParser.fromRecord
    { moduleName:
        ArgParser.argument [ "--main" ]
          "Module to be used as the application's entry point.\n\
          \Defaults to 'Main'."
          # ArgParser.default "Main"
    , libDir:
        ArgParser.argument [ "--libdir" ]
          "Path to scheme source files.\n\
          \Defaults to './output'."
          # ArgParser.default (Path.concat [ ".", "output" ])
    , outputDir:
        ArgParser.argument [ "--output-dir" ]
          "Path to output directory for backend files.\n\
          \Defaults to './output'."
          # ArgParser.default (Path.concat [ ".", "output" ])
    }

runCmdArgParser :: ArgParser RunArgs
runCmdArgParser =
  ArgParser.fromRecord
    { moduleName:
        ArgParser.argument [ "--main" ]
          "Module to be used as the application's entry point.\n\
          \Defaults to 'Main'."
          # ArgParser.default "Main"
    , libDir:
        ArgParser.argument [ "--libdir" ]
          "Path to scheme source files.\n\
          \Defaults to './output'."
          # ArgParser.default (Path.concat [ ".", "output" ])
    }

main :: FilePath -> Effect Unit
main cliRoot = do
  cliArgs <- Array.drop 2 <$> Process.argv
  case
    ArgParser.parseArgs "purescm" "Chez Scheme backend for PureScript" cliArgParser
      cliArgs
    of
    Left err ->
      Console.error $ ArgParser.printArgError err
    Right (Build args) ->
      flip runAff_ (runBuild args) case _ of
        Right _ -> pure unit
        Left err -> do
          Console.error (message err)
          Process.exit' 1
    Right (Bundle arg) ->
      launchAff_ $ runBundle cliRoot arg
    Right (Run arg) ->
      launchAff_ $ runRun cliRoot arg

runBuild :: BuildArgs -> Aff Unit
runBuild args = do
  let runtimePath = Path.concat [ args.outputDir, "purs", "runtime" ]
  mkdirp runtimePath
  let runtimeFilePath = Path.concat [ runtimePath, moduleLib <> schemeExt ]
  let runtimeContents = Dodo.print plainText Dodo.twoSpaces $ Printer.printLibrary $ runtimeModule
  FS.writeTextFile UTF8 runtimeFilePath runtimeContents
  basicBuildMain
    { coreFnDirectory: args.coreFnDir
    , coreFnGlobs: pure "**"
    , externalDirectivesFile: args.directivesFile
    , onCodegenModule: \_ (Module { name: ModuleName name, path }) backend _ -> do
        let
          formatted =
            Dodo.print plainText Dodo.twoSpaces
              $ Printer.printLibrary
              $ codegenModule backend
        let modPath = Path.concat [ args.outputDir, name ]
        mkdirp modPath
        let libPath = Path.concat [ modPath, moduleLib <> schemeExt ]
        FS.writeTextFile UTF8 libPath formatted
        unless (Set.isEmpty backend.foreign) do
          let
            foreignSiblingPath =
              fromMaybe path (String.stripSuffix (Pattern (Path.extname path)) path) <>
                schemeExt
          let foreignOutputPath = Path.concat [ modPath, moduleForeign <> schemeExt ]
          res <- attempt $ copyFile foreignSiblingPath foreignOutputPath
          unless (isRight res) do
            Console.log $ "  Foreign implementation missing."
    , onPrepareModule: \build coreFnMod@(Module { name }) -> do
        let total = show build.moduleCount
        let index = show (build.moduleIndex + 1)
        let padding = power " " (SCU.length total - SCU.length index)
        Console.log $ Array.fold
          [ "[", padding, index, " of ", total, "] purescm: building ", unwrap name ]
        pure coreFnMod
    }

runBundle :: FilePath -> BundleArgs -> Aff Unit
runBundle cliRoot args = do
  let outPath = Path.concat [ args.outputDir, "main" ]
  let mainPath = Path.concat [ args.outputDir, "main.ss" ]
  let mainWpoPath = Path.concat [ args.outputDir, "main.wpo" ]
  mkdirp args.outputDir
  let
    mainContent = Array.fold
      [ "(import (" <> args.moduleName <> " lib))"
      , "(main)"
      ]
  FS.writeTextFile UTF8 mainPath mainContent
  let
    runtimePath = Path.concat [ cliRoot, "vendor" ]
    runtimeLibPathPair = runtimePath <> "::" <> (Path.concat [ args.outputDir ])
    libDirPathPair = args.libDir <> "::" <> args.outputDir
    libDirs = runtimeLibPathPair <> ":" <> libDirPathPair <> ":"
    arguments = [ "-q", "--libdirs", libDirs ]
  res <- evalScheme arguments $ Array.fold
    [ "(top-level-program (import (chezscheme))"
    , "  (with-exception-handler (lambda (e) (display-condition e (console-error-port)) (newline (console-error-port)) (exit -1))"
    , "    (lambda ()"
    , "      (optimize-level 3)"
    , "      (compile-file-message #f)"
    , "      (compile-imported-libraries #t)"
    , "      (generate-wpo-files #t)"
    , "      (compile-program \"" <> mainPath <> "\")"
    , "      (compile-whole-program \"" <> mainWpoPath <> "\" \"" <> outPath <> "\")"
    , ")))"
    ]
  case res.exitCode of
    Just 0 -> Console.log $ "Created " <> outPath
    _ -> liftEffect $ Process.exit' (fromMaybe 1 res.exitCode)

runRun :: FilePath -> RunArgs -> Aff Unit
runRun cliRoot args = do
  let
    runtimePath = Path.concat [ cliRoot, "vendor" ]
    libDirs = runtimePath <> ":" <> args.libDir <> ":"
    arguments = [ "-q", "--libdirs", libDirs ]
  res <- evalScheme arguments $ Array.fold
    [ "(base-exception-handler (lambda (e) (display-condition e (console-error-port)) (newline (console-error-port)) (exit -1)))"
    , "(top-level-program (import (" <> args.moduleName <> " lib)) (main))"
    ]
  case res.exitCode of
    Just 0 -> pure unit
    _ -> liftEffect $ Process.setExitCode (fromMaybe 1 res.exitCode)

evalScheme :: Array String -> String -> Aff ExecaResult
evalScheme arguments code = do
  schemeBin <- getSchemeBinary
  spawned <- execa schemeBin arguments identity
  spawned.stdin.writeUtf8End code
  spawned.stdout.pipeToParentStdout
  spawned.stderr.pipeToParentStderr
  spawned.getResult

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

mkdirp :: FilePath -> Aff Unit
mkdirp path = FS.mkdir' path { recursive: true, mode: mkPerms Perms.all Perms.all Perms.all }

copyFile :: FilePath -> FilePath -> Aff Unit
copyFile from to = do
  stats <- FS.stat from
  unless (Stats.isFile stats) do
    throwError $ error $ "Not a file: " <> from
  makeAff \k -> do
    src <- createReadStream from
    dst <- createWriteStream to
    src # on_ Stream.errorH \err -> do
      Stream.destroy' dst $ error $ "Got error in src stream: " <> message err
      k $ Left err
    dst # on_ Stream.errorH (k <<< Left)
    dst # on_ Stream.finishH (k (Right unit))
    Stream.pipe src dst
    pure $ effectCanceler do
      Stream.destroy dst
      Stream.destroy src
