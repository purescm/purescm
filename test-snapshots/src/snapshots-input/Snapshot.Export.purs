module Snapshot.Export (exported, foo, Foo, bar, Bar, Test, exportedForeign) where

data Foo = Foo

data Bar = Bar Int Int

data Test = Zero | One Int | Two Int Int

exported :: Test
exported = Two 1 2

foo :: Foo
foo = Foo

bar :: Bar
bar = Bar 1 2

foreign import exportedForeign :: Int

foreign import internalForeign :: Int
