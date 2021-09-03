module PureScheme.Test.Binder.Constructor where

data Baz = Qux Int

foo (Qux 1) = Qux 2
foo (Qux a@i) = Qux a
foo (Qux _) = Qux 3
