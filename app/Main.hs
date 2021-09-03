module Main where

import qualified System.IO as IO
import System.Environment (getArgs)
import Control.Exception (SomeException, try)
import Data.Foldable (for_)
import qualified Data.Text as Text
import Options (Options(..))
import qualified Options as Options
import Language.PureScript.CoreFn.Module (Module(..))
import Language.PureScript.Scheme.IOUtil (print')
import qualified Language.PureScript.Scheme.IOUtil as IOUtil
import qualified Language.PureScript.Scheme.Make as Make
import qualified Language.PureScript.Scheme.CodeGen.Printer as Printer
import Language.PureScript.Scheme.CodeGen.Library (Library(..))

main :: IO ()
main = do
  IO.hSetEncoding IO.stdout IO.utf8
  IO.hSetEncoding IO.stderr IO.utf8
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering

  Options { optionOutput } <- Options.runParser =<< getArgs
  print' $ "Output directory is " <> optionOutput

  corefnFiles <- Make.findCorefnFiles optionOutput

  for_ corefnFiles $ \corefnFile -> do
    print' $ "Processing " <> corefnFile

    print' $ "    Reading module"
    module_ <- Make.readModuleFromJSON corefnFile
    print' $ "    Module read successfully"

    let modulePath' = Text.pack $ modulePath module_
    print' $ "    Module source path is " <> modulePath'

    print' $ "    Compiling module"
    let library = Make.compile module_
    print' $ "    Module compiled successfully"

    let libraryDir = optionOutput <> "/" <> (libraryName library) 
    print' $ "    Destination directory is " <> libraryDir

    let libraryPath = libraryDir <> "/lib.sls"
    print' $ "    Destination library path is " <> libraryPath

    print' $ "    Writing library to destination path"
    IOUtil.writeTextFile libraryPath (Printer.printLibrary library)
    print' $ "    Library written to destination path"

    if null (libraryForeigns library) then
      print' $ "    Library does not require foreigns"
    else do
      print' $ "    Library requires foreigns"

      let foreignSourcePath = IOUtil.changeFileExtension modulePath' "sls"
      print' $ "    Foreign source path is " <> foreignSourcePath

      let foreignPath = libraryDir <> "/foreign.sls"
      print' $ "    Destination Foreign path is " <> foreignPath

      print' $ "    Copying foreign library"
      try (IOUtil.cp foreignSourcePath foreignPath) >>= \case
        Left (_err :: SomeException)
          -> print' "    Could not copy foreign library"
        Right _ -> print' "    Foreign library copied successfully"

    print' ""
