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
