module Snapshot.UncurriedFunction where

import Data.Function.Uncurried (mkFn0, mkFn2, runFn0, runFn2)

test1a = mkFn0 \_ -> 1

test1b = runFn0

test2a = mkFn2 \a _ -> a

test2b = runFn2 test2a 1 2

test3a = mkFn2 \_ b -> b

test3b = runFn2 test3a 1 2
