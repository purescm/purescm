module Snapshot.Export (exported, Test, exportedForeign) where

exported :: Test
exported = Baz 1 2

data Test = Foo | Bar Int | Baz Int Int

foreign import exportedForeign :: Int

foreign import internalForeign :: Int
