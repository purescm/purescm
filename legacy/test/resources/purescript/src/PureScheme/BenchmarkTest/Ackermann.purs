module PureScheme.BenchmarkTest.Ackermann where

import Prelude

test :: Int -> Int -> Int 
test 0 y = y + 1
test x 0 = test (x - 1) 1
test x y = test (x - 1) $ test x (y - 1)
