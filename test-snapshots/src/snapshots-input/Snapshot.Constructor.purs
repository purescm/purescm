module Snapshot.Constructor where

data Maybe a = Just a | Nothing

data Tree a
  = Nil
  | Node (Tree a) a (Tree a)

-- recursive constructors
data Mutual1
  = Stop1
  | Continue1 Mutual2

data Mutual2
  = Stop2
  | Continue2 Mutual1
