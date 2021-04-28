module Main where
import qualified PureScm.CoreFn as CoreFn

main = CoreFn.readJSONFile "resources/corefn.json"
