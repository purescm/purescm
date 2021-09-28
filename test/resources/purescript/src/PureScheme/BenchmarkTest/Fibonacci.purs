module PureScheme.BenchmarkTest.Fibonacci where

import Prelude

test :: Int -> Int
test 0 = 0
test 1 = 1
test n = test (n - 1) + test (n - 2)
