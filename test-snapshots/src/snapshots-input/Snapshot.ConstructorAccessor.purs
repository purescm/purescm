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

data OneArg = OneArg Int

one :: OneArg
one = OneArg 1

test2 :: OneArg -> Int
test2 = case _ of
  OneArg i -> i

data HasArgs = HasArgs Int Int Int

test3 :: HasArgs -> Int
test3 = case _ of
  HasArgs i1 _ _ -> i1

test4 :: HasArgs -> Int
test4 = case _ of
  HasArgs i1 i2 i3 
    | i1 < i3 -> i1
    | otherwise -> i2

data SumWithArgs
  = First Int
  | Last Int

test5 :: SumWithArgs -> Int
test5 = case _ of
  First i -> i
  Last i -> i

test6 :: Partial => SumWithArgs -> Int
test6 = case _ of
  First i -> i
