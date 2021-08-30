module PureScheme.Test.Record.Accessor where

foo :: { bar :: Int, baz :: Int }
foo = { bar: 23
      , baz: 42
      }

bar = foo.bar
baz = foo.baz