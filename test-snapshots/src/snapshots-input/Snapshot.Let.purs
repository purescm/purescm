module Snapshot.Let where

import Prelude

letChain :: Int -> Int
letChain x =
  let
    a = x + x
    b = a + a
    c = b + b
    d = c * c
  in
    a + b + c + d

letRecursive :: Int -> Int
letRecursive x = go x
  where
  go 0 = 0
  go x' = go (x' - 1)

isEven :: Int -> Boolean
isEven x
  | x == 0 = true
  | otherwise = isOdd (x - 1)

isOdd :: Int -> Boolean
isOdd x
  | x == 1 = false
  | otherwise = isEven (x - 1)
