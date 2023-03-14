module PureScheme.Test.Guard.Min where

import Prelude

min :: Int -> Int -> Int
min n m
  | n < m     = n
  | otherwise = m
