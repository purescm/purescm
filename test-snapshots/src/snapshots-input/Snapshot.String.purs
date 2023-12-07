module Snapshot.String where

import Prelude
import Effect (Effect)

foreign import testStringMain :: Effect Unit

main :: Effect Unit
main = do
  testStringMain
