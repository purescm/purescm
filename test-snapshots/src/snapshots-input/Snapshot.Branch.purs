module Snapshot.Branch where

import Partial.Unsafe (unsafePartial)

f :: Boolean -> Boolean -> Boolean -> Int
f x y z = if x then if y then if z then 0 else 1 else 2 else 3

g :: Int -> Int
g = case _ of
  0 -> 1
  1 -> 2
  2 -> 3
  _ -> 0

h :: Number -> Number
h = unsafePartial case _ of
  3.14 -> 3.14159

i :: Boolean -> Boolean -> Boolean
i = case _, _ of
  true, true -> false
  false, false -> true
  false, true -> false
  true, false -> true
