module Main where

import           Data.Text                                     (Text)
import           Data.Text.IO                                  as T (putStrLn)
import qualified Data.Aeson                                    as JSON
import qualified Data.Aeson.Types                              as JSON.T
import qualified Language.PureScript.CoreFn.FromJSON           as CoreFn
import           Language.PureScript.CoreFn.Module             (Module)
import           Language.PureScript.CoreFn.Ann                (Ann)
import           Language.PureScript.Scheme.CodeGen.Transpiler (moduleToScheme)
import           Language.PureScript.Scheme.CodeGen.Printer    (printScheme)
import           Language.PureScript.Scheme.CodeGen.Optimizer  (runOptimizations)

main :: IO ()
main = compile "resources/PureScmTest.Fib.corefn.json" >>= T.putStrLn

compile :: FilePath -> IO Text
compile path = (printScheme . runOptimizations . moduleToScheme)
           <$> readJSONFile path

readJSONFile :: FilePath -> IO (Module Ann)
readJSONFile path = do
  r <- JSON.decodeFileStrict' path
  let res = r >>= JSON.T.parseMaybe CoreFn.moduleFromJSON
  case res of
    Nothing -> error "Nope"
    Just (_version, module_) -> return module_
