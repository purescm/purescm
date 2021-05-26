module PureScmTest.NestedConstructor where

data Foo = Foo Int

data Bar = Bar Foo

match :: Bar -> Int
match (Bar (Foo 0)) = 1
match _ = 23
