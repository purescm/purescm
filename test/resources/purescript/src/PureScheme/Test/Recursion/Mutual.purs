module PureScheme.Test.Recursion.Mutual where

foo :: Int -> Int -> Int
foo 0 n = foo 1 n
foo 1 n = bar 2 n
foo 2 n = n
foo _ _ = 0

bar :: Int -> Int -> Int
bar 0 m = bar 1 m
bar 1 m = foo 2 m
bar 2 m = m
bar _ _ = 0
