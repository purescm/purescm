module Snapshot.Branch where

f :: Boolean -> Boolean -> Boolean -> Int
f x y z = if x then if y then if z then 0 else 1 else 2 else 3

g :: Int -> Int
g = case _ of
  0 -> 1
  1 -> 2
  2 -> 3
  _ -> 0

h :: Number -> Number
h = case _ of
  3.14 -> 3.14159
  _ -> 0.0
