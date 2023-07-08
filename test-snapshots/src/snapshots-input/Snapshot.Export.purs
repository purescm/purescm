module Snapshot.Export (exported, Test) where

exported :: Test
exported = Baz 1 2

data Test = Foo Int | Bar | Baz Int Int
