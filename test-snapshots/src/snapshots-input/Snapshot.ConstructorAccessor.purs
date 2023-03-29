module Snapshot.ConstructorAccessor where

import Prelude

data Foo = Foo Int Int Int

-- A constructor that is partially applied to its arguments.
x :: Int -> Int -> Foo 
x = Foo 1

y :: Int -> Foo
y = Foo 1 1

z :: Foo
z = Foo 1 1 1

-- more pattern matching tests
data NoArgs = NoArgs

test1 :: NoArgs -> Boolean
test1 = case _ of
  NoArgs -> true

data HasArgs = HasArgs Int Int Int

test2 :: HasArgs -> Int
test2 = case _ of
  HasArgs i1 _ _ -> i1

test3 :: HasArgs -> Int
test3 = case _ of
  HasArgs i1 i2 i3 
    | i1 < i3 -> i1
    | otherwise -> i2

data SumWithArgs
  = First Int
  | Last Int

test4 :: SumWithArgs -> Int
test4 = case _ of
  First i -> i
  Last i -> i

test5 :: Partial => SumWithArgs -> Int
test5 = case _ of
  First i -> i
