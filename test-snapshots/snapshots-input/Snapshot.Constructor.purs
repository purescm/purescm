module Snapshot.Constructor where

data Maybe a = Just a | Nothing

data Tree a
  = Nil
  | Node (Tree a) a (Tree a)

nothing :: Maybe Int
nothing = Nothing

just1 :: Maybe Int
just1 = Just 1

tree :: Tree Int
tree = Node (Node (Node Nil 1 Nil) 2 (Node (Node Nil 3 Nil) 4 Nil)) 5 Nil

extractInt :: Maybe Int -> Int
extractInt (Just i) = i
extractInt Nothing = 0
