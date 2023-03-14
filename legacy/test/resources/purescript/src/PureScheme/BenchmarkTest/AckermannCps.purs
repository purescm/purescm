module PureScheme.BenchmarkTest.AckermannCps where

import Prelude

test :: Int -> Int -> Int
test x y = go x y identity
  where
    go :: Int -> Int -> (Int -> Int) -> Int
    go 0 n cont = cont (n + 1)
    go m 0 cont = go (m - 1) 1 cont
    go m n cont = go m (n - 1) (\a -> go (m - 1) a cont)

