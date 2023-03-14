module PureScheme.Test.Record.UpdateInLet where

antani =
  let foo = { bar: 23
            , baz: 42
            }
  in
    foo { bar = 69
        }
