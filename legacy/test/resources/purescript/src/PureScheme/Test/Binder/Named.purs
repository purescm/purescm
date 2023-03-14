module PureScheme.Test.Binder.Named where

data Foo = Bar Int
         | Baz

match bar@(Bar i) = bar
match Baz = Baz
