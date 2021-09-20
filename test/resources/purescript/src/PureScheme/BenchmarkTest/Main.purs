module PureScheme.BenchmarkTest.Main where

import Prelude
import Performance.Minibench (bench)

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

main = bench (\_ -> fibonacci 5)
