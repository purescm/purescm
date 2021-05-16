module PureScmTest.GuardedFunction where

import Prelude

min :: Int -> Int -> Int
min n m
  | n < m     = n
  | otherwise = m

gcd :: Int -> Int -> Int
gcd n 0 = n
gcd 0 n = n
gcd n m | n > m     = gcd (n - m) m
        | otherwise = gcd n (m - n)
