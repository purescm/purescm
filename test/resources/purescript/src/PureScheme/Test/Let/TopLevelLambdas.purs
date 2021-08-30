module PureScheme.Test.Let.TopLevelLambdas where

import Prelude

foo =
  let
    add x y = x + y
    sub x y = x - y
  in
    add (sub 1 2) (add 3 4)
