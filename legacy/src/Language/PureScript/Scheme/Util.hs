module Language.PureScript.Scheme.Util where

mapWithIndex :: (Integer -> a -> b) -> [a] -> [b]
mapWithIndex f l = zipWith f [0 ..] l

concatMapWithIndex :: (Integer -> a -> [b]) -> [a] -> [b]
concatMapWithIndex f l = concat $ mapWithIndex f l
