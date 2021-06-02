module PureScheme.Test.Constructor where

data Foo = Bar Int Int
         | Baz

match (Bar 1 2)   = 0
match (Bar 1 _)   = 1
match (Bar _ 2)   = 2
match (Bar i1 i2) = i1
match Baz = 0
