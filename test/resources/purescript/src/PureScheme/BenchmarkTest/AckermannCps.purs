module PureScheme.BenchmarkTest.AckermannCps where

import Prelude

test :: Int -> Int -> (Int -> Int) -> Int
test 0 n cont = cont (n + 1)
test m 0 cont = test (m - 1) 1 cont
test m n cont = test m (n - 1) (\a -> test (m - 1) a cont)
