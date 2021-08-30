module PureScheme.Test.Let.LambdasInFunction where

import Prelude

foo x y =
  let
    add a b = x + y
    sub a b = x - y
  in
    add (sub x y) (add x y)