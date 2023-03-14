module PureScheme.BenchmarkTest.FibonacciTco where

import Prelude

test :: Int -> Int
test n = fib n 0 1
  where
    fib 0 a b = a
    fib n a b = fib (n - 1) b (a + b)
