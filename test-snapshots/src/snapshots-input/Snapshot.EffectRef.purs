-- @inline Snapshot.EffectRef.onLet never
module Snapshot.EffectRef where

import Prelude

import Effect (Effect)
import Effect.Ref (Ref, modify_, new, read)
import Test.Assert (assert)

positionZero :: Effect (Ref Int)
positionZero = new 0

onLet :: Int -> Effect (Ref Int)
onLet x =
  let
    a = x + x
    b = a + x
  in
    new $ a + b

basicTest :: Effect Unit
basicTest = do
  n <- new 0
  modify_ (_ + 1) n
  v <- read n
  assert $ v == 1

onLetTest :: Effect Unit
onLetTest = do
  n <- onLet 1
  v <- read n
  assert $ v == 5

main :: Effect Unit
main = do
  basicTest
  onLetTest
