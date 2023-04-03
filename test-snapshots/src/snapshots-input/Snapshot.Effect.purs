-- @inline Snapshot.Effect.dontInlineMe never
module Snapshot.Effect where

import Prelude

import Effect (Effect)

dontInlineMe :: forall a. a -> Effect Unit
dontInlineMe _ = pure unit

main :: Effect Unit
main = do
  dontInlineMe "a"
  dontInlineMe "b"
  value <- dontInlineMe "c"
  dontInlineMe "d"
  dontInlineMe "e"
  pure value
