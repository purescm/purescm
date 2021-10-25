module PureScheme.Test.Guard.Take where

import Prelude

data List a = Nil | Cons a (List a)

infixr 6 Cons as :

take :: forall a. Int -> List a -> List a
take = go Nil
  where
  go acc n _ | n < 1 = acc
  go acc _ Nil = acc
  go acc n (x : xs) = go (x : acc) (n - 1) xs
