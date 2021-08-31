module Language.PureScript.Scheme.Make where

import           Data.Text                                     (Text)
import qualified Data.Aeson                                    as JSON
import qualified Data.Aeson.Types                              as JSON.T
import qualified Language.PureScript.CoreFn.FromJSON           as CoreFn
import           Language.PureScript.CoreFn.Module             (Module)
import           Language.PureScript.CoreFn.Ann                (Ann)
import           Language.PureScript.Scheme.CodeGen.Transpiler (moduleToLibrary)
import           Language.PureScript.Scheme.CodeGen.Printer    (printLibrary)
import           Language.PureScript.Scheme.CodeGen.Optimizer  (runOptimizations)
import           Language.PureScript.Scheme.CodeGen.Library    (Library(..))

compile :: FilePath -> IO Text
compile path = (printLibrary . optimize . moduleToLibrary)
           <$> readModuleFromJSON path

optimize :: Library -> Library
optimize library = library { libraryBody = runOptimizations $ libraryBody library }

readModuleFromJSON :: FilePath -> IO (Module Ann)
readModuleFromJSON path = do
  r <- JSON.decodeFileStrict' path
  let res = r >>= JSON.T.parseMaybe CoreFn.moduleFromJSON
  case res of
    Nothing -> error "Error reading CoreFn file."
    Just (_version, module_) -> return module_
