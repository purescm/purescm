module PureScheme.Test.Recursion.Fibonacci where

import Prelude

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
