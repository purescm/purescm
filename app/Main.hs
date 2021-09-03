module Main where

import Data.Foldable (for_)
import qualified Data.Text as Text
import Language.PureScript.CoreFn.Module (Module(..))
import Language.PureScript.Scheme.IOUtil (print')
import qualified Language.PureScript.Scheme.IOUtil as IOUtil
import qualified Language.PureScript.Scheme.Make as Make
import qualified Language.PureScript.Scheme.CodeGen.Printer as Printer
import Language.PureScript.Scheme.CodeGen.Library (Library(..))

main :: IO ()
main = do
  let outputPath = "test/resources/purescript/output"
  corefnFiles <- Make.findCorefnFiles outputPath

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

    let libraryDir = outputPath <> "/" <> (libraryName library) 
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
      IOUtil.cp foreignSourcePath foreignPath
      print' $ "    Foreign library copied successfully"

    print' ""
