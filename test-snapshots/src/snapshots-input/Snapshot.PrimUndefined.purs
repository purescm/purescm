-- @inline Snapshot.PrimUndefined.testCase never
module Snapshot.PrimUndefined where

import Prelude

import Effect (Effect)
import Test.Assert (assert)

-- One place where PrimUndefined appears is to get the instance dictionary
-- for a superclass, which is hidden behind a `(lambda (_) ..)` abstraction
testCase :: forall a. Ring a => a -> a -> a
testCase x y = x + y

main :: Effect Unit
main = assert $ testCase 1 1 == 2
