module Snapshot.EffectRef where

import Prelude

import Effect (Effect)
import Effect.Ref (modify_, new, read)
import Test.Assert (assert)

example = new 0

main :: Effect Unit
main = do
  n <- new 0
  modify_ (_ + 1) n
  v <- read n
  assert $ v == 1
