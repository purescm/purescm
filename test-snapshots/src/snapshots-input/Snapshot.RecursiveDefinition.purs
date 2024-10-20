-- This example is from <https://github.com/purescript/purescript/blob/0527e71725257bf16a3c7bd5fce608e856a77dae/tests/purs/optimize/4179.purs>
module Snapshot.RecursiveDefinition where

import Prelude
import Effect (Effect)

alpha :: Int -> Int -> Number
alpha = case _ of
  0 -> bravo
  1 -> charlie
  2 -> \y -> if y > 0 then bravo y else charlie y
  x -> \y -> delta y x

bravo :: Int -> Number
bravo = (\_ -> alpha) {} 3

charlie :: Int -> Number
charlie = (\_ -> alpha) {} 4

delta :: Int -> Int -> Number
delta =
  let b = (\_ -> bravo) {}
  in \x y -> if x == y then b 0 else 1.0

main :: Effect Unit
main = pure unit
