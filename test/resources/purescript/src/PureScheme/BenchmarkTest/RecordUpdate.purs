module PureScheme.BenchmarkTest.RecordUpdate where

import Prelude

type R = { a :: Int }

test :: Int -> R
test n = go { a: 0 } 1
  where
    go :: R -> Int -> R
    go acc x = if x < n
               then go (acc { a = x }) (x + 1)
               else acc
