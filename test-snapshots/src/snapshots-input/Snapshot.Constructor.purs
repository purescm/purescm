module Snapshot.Constructor where

data Maybe a = Just a | Nothing

data Tree a
  = Nil
  | Node (Tree a) a (Tree a)
