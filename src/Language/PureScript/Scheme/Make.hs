module Language.PureScript.Scheme.Make where

import Data.Text (Text)
import Language.PureScript.CoreFn.FromJSON (moduleFromJSON)
import Language.PureScript.CoreFn.Module (Module(..))
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.Scheme.IOUtil (findEnds, readJSONFile)
import Language.PureScript.Scheme.CodeGen.Transpiler (moduleToLibrary)
import Language.PureScript.Scheme.CodeGen.Optimizer (runOptimizations)
import Language.PureScript.Scheme.CodeGen.Library (Library(..))

findCorefnFiles :: Text -> IO [Text]
findCorefnFiles outputDir = findEnds outputDir "corefn.json"

readModuleFromJSON :: Text -> IO (Module Ann)
readModuleFromJSON path = do
  (_version, module_) <- readJSONFile path moduleFromJSON
  return module_

compile :: Module Ann -> Library
compile module_
   = optimize
   $ removePrimImports
   $ moduleToLibrary
   $ module_

optimize :: Library -> Library
optimize library = library { libraryBody = runOptimizations $ libraryBody library }

removePrimImports :: Library -> Library
removePrimImports library
  = library { libraryImports = filter go $ libraryImports library }
  where
    go "Prim" = False
    go _ = True
