module PureScheme.Test.Binder.Mixed where

import Prelude

foo 0 0 = 0
foo 0 1 = 1
foo 1 0 = 10
foo _ 2 = 2
foo 3 _ = 30
foo 4 n = n
foo m 5 = m
foo m n = m + n
