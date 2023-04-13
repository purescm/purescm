-- @inline Snapshot.UncurriedFunction.pure' never
module Snapshot.UncurriedFunction where

import Prelude

import Data.Function.Uncurried (Fn0, Fn2, mkFn0, mkFn2, runFn0, runFn2)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Console (log)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1)
import Test.Assert (assert)

test1a :: Fn0 Int
test1a = mkFn0 \_ -> 1

test1b :: Int
test1b = runFn0 test1a

test2a :: forall a b. Fn2 a b a
test2a = mkFn2 \a _ -> a

test2b :: Int
test2b = runFn2 test2a 1 2

test3a :: forall a b. Fn2 a b b
test3a = mkFn2 \_ b -> b

test3b :: Int
test3b = runFn2 test3a 1 2

test4a :: EffectFn1 String String
test4a = mkEffectFn1 $ \a -> do 
  log a
  pure a

test4b :: Effect String
test4b = runEffectFn1 test4a "test4b"

main :: Effect Unit
main = do
  assert $ test1b == 1
  assert $ test2b == 1
  assert $ test3b == 2

  v <- test4b
  assert $ v == "test4b"

  w <- runEffectFn1 test4a "test4b"
  assert $ w == "test4b"
