module PureScheme.Test.Let.ValuesInFunction where

import Prelude

foo x y =
  let a = 1
      b = 2
  in
    (x + a) - (y + b)
