module PureScheme.Test.Record.UpdateOpenRow where

type Foo a = Record ( bar :: Int | a )

foo :: forall a. Foo a -> Foo a
foo = _ { bar = 42 }
