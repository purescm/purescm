module Snapshot.List where

import Prelude

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Test.Assert (assert)

xs :: List Int
xs = Cons 1 Nil

curried :: Int -> List Int -> List Int
curried = Cons

cons2 :: List Int -> List Int
cons2 = Cons 2

car :: List Int -> Maybe Int
car Nil = Nothing
car (Cons x _) = Just x

cdr :: List Int -> Maybe (List Int)
cdr Nil = Nothing
cdr (Cons _ xs') = Just xs'

main :: Effect Unit
main = do
  let l = 1 : 2 : Nil
  assert $ car l == Just 1
  assert $ cdr l == Just (2 : Nil)
  assert $ curried 1 Nil == 1 : Nil
  assert $ cons2 (3 : Nil) == 2 : 3 : Nil
  assert $ show l == "(1 : 2 : Nil)"
