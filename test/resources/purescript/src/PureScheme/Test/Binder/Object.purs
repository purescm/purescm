module PureScheme.Test.Binder.Object where

foo {a: 1, b: 2, c: 3} = 4
foo {d: 5, e: 6} = 7
foo {} = 8
