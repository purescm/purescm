module Main where

import           Data.Text.IO                    as T (putStrLn)
import           Language.PureScript.Scheme.Make      (compile)

main :: IO ()
main = compile "resources/PureScmTest.Fib.corefn.json" >>= T.putStrLn
