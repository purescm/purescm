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

letChainRecursive :: Int -> Array Boolean
letChainRecursive n =
  let
    isEven' x = if x == 0 then true else isOdd' (x - 1)
    isOdd' x = if x == 0 then false else isEven' (x - 1)
  in
    [ isEven' n, isOdd' n ]

letChainMix :: Int -> Array Boolean
letChainMix n =
  let
    isEven' x = if x == 0 then true else isOdd' (x - 1)
    isOdd' x = if x == 0 then false else isEven' (x - 1)

    o = n * 2
    p = o * 2
  in
    [ isEven' n, isOdd' n, isEven' o, isOdd' o, isEven' p, isOdd' p ]

letRecursive :: Int -> Int
letRecursive x
  | x == 0 = 0
  | otherwise = letRecursive (x - 1)

isEven :: Int -> Boolean
isEven x
  | x == 0 = true
  | otherwise = isOdd (x - 1)

isOdd :: Int -> Boolean
isOdd x
  | x == 0 = false
  | otherwise = isEven (x - 1)
