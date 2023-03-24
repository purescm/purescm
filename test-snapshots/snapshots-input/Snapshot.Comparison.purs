module Snapshot.Comparison where

import Prelude

integerComparison :: Int -> Int -> Array Boolean
integerComparison x y =
  [ x == y
  , x /= y
  , x < y
  , x <= y
  , x >= y
  , x > y
  ]

booleanComparison :: Boolean -> Boolean -> Array Boolean
booleanComparison x y =
  [ x == y
  , x /= y
  , x < y
  , x <= y
  , x >= y
  , x > y
  ]

numberComparison :: Number -> Number -> Array Boolean
numberComparison x y =
  [ x == y
  , x /= y
  , x < y
  , x <= y
  , x >= y
  , x > y
  ]

charComparison :: Char -> Char -> Array Boolean
charComparison x y =
  [ x == y
  , x /= y
  , x < y
  , x <= y
  , x >= y
  , x > y
  ]

stringComparison :: String -> String -> Array Boolean
stringComparison x y =
  [ x == y
  , x /= y
  , x < y
  , x <= y
  , x >= y
  , x > y
  ]
