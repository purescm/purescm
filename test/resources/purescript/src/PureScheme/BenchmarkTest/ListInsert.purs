module PureScheme.BenchmarkTest.ListInsert where

import Prelude

data List a = Nil
            | Cons a (List a)

test n = go Nil 0
  where
    go :: List Int -> Int -> List Int
    go acc x = if x < n
               then go (Cons x acc) (x + 1)
               else acc
