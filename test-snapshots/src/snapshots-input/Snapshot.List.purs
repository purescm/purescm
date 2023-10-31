module Snapshot.List where

import Data.List (List(..))
import Data.Maybe (Maybe(..))

xs :: List Int
xs = Cons 1 Nil

curried :: Int -> List Int -> List Int
curried = Cons

car :: List Int -> Maybe Int
car Nil = Nothing
car (Cons x _) = Just x

cdr :: List Int -> Maybe (List Int)
cdr Nil = Nothing
cdr (Cons _ xs') = Just xs'
