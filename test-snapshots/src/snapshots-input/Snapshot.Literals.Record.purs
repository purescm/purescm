-- @inline Snapshot.Literals.Record.recordAccess never
-- @inline Snapshot.Literals.Record.recordUpdate never
module Snapshot.Literals.Record where

import Prelude

import Effect (Effect)
import Test.Assert (assert)

recordAccess :: forall a r. { fooBarBaz :: a | r } -> a
recordAccess = _.fooBarBaz

recordUpdate :: forall r. { fooBarBaz :: Int | r } -> { fooBarBaz :: Int | r }
recordUpdate = _ { fooBarBaz = 10 }

main :: Effect Unit
main = do
  let r = { fooBarBaz: 5 }
  let s = recordUpdate r
  
  assert $ recordAccess s == 10
  assert $ recordAccess r == 5
