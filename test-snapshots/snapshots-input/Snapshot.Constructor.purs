module Snapshot.Constructor where

data Maybe a = Just a | Nothing

data Tree a
  = Nil
  | Node (Tree a) a (Tree a)

extractInt :: Maybe Int -> Int
extractInt (Just i) = i
extractInt Nothing = 0
