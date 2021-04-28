module PureScm.CoreFn where

import qualified Data.Aeson                          as JSON
import qualified Data.Aeson.Types                    as JSON.T
import qualified Language.PureScript.CoreFn.FromJSON as CoreFn

readJSONFile :: FilePath -> IO ()
readJSONFile path = do
  r <- JSON.decodeFileStrict' path
  let res = r >>= JSON.T.parseMaybe CoreFn.moduleFromJSON
  case res of
    Nothing -> error "Nope"
    Just x -> putStrLn $ show x
