module Snapshot.ArrayIndex where

testAccessorGetIndex :: Array Int -> Int
testAccessorGetIndex = case _ of
  [ x ] -> x
  _ -> 0
