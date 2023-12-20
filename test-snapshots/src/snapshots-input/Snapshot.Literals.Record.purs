-- @inline Snapshot.Literals.Record.recordAccess never
-- @inline Snapshot.Literals.Record.recordUpdate never
-- @inline Snapshot.Literals.Record.recordAddField never
module Snapshot.Literals.Record where

import Prelude

import Effect (Effect)
import Prim.Row (class Lacks)
import Record as Record
import Test.Assert (assert, assertThrows)
import Type.Proxy (Proxy(..))

recordAccess :: forall a r. { fooBarBaz :: a | r } -> a
recordAccess = _.fooBarBaz

recordUpdate :: forall r. { fooBarBaz :: Int | r } -> { fooBarBaz :: Int | r }
recordUpdate = _ { fooBarBaz = 10 }

recordAddField :: forall r. Lacks "anotherField" r => { fooBarBaz :: Int | r } -> { fooBarBaz :: Int, anotherField :: Int | r }
recordAddField = Record.insert (Proxy :: _ "anotherField") 42

foreign import minusTwo :: Int

foreign import data Foreign :: Type

foreign import unsafeGetNotFound :: forall r. Record r -> Foreign

main :: Effect Unit
main = do
  let r = { fooBarBaz: 5 }
  let s = recordUpdate r
  let t = recordAddField s
  let u = { fooBarBaz: minusTwo }

  assert $ recordAccess t == 10
  assert $ t.anotherField == 42
  assert $ recordAccess s == 10
  assert $ recordAccess r == 5
  assert $ recordAccess u == minusTwo
  assertThrows $ \_ -> unsafeGetNotFound u
