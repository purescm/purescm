module PureScheme.Test.Let.TopLevelValues where

import Prelude

foo =
  let
    a = 1
    b = 2
  in
    a + b
