module Main where
import qualified PureScm.CoreFn as CoreFn

main :: IO ()
main = CoreFn.readJSONFile "resources/corefn.json"
