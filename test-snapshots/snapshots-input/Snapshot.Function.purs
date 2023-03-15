-- @inline Snapshot.Function.f never
module Snapshot.Function where

f :: Int -> Int -> Array Int
f x y = [ x, y, x, y, x ]

g :: Int -> Int -> Array Int
g x y = f x y
