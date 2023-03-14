module PureScheme.Test.Binder.Guard where

import Prelude

match 1 a
  | a == 2 = 3
  | a == 4 = 5
  | otherwise = 6