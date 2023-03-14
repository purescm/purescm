module PureScheme.Test.Record.UpdateClosedRow where

foo :: { bar :: Int, baz :: Int }
foo = { bar: 23
      , baz: 42
      }

foo' = foo { bar = 69
           , baz = 420
           }
