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
   $ filterImports
   $ moduleToLibrary
   $ module_

optimize :: Library -> Library
optimize library = library { libraryBody = runOptimizations $ libraryBody library }

filterImports :: Library -> Library
filterImports library@Library{libraryName, libraryImports}
  = library { libraryImports = filter filterModule libraryImports }
  where
    filterModule "Prim" = False
    filterModule m | m == libraryName = False
    filterModule _ = True
