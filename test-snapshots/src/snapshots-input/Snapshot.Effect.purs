-- @inline Snapshot.Effect.dontInlineMe never
module Snapshot.Effect where

import Prelude

import Effect (Effect)

dontInlineMe :: forall a. a -> Effect Unit
dontInlineMe _ = pure unit

lastComponentIsRun :: Effect Unit
lastComponentIsRun = do
  dontInlineMe "a"
  dontInlineMe "b"
  dontInlineMe "c"

lastPureIsUnwrapped :: Effect Unit
lastPureIsUnwrapped = do
  value <- dontInlineMe "a"
  dontInlineMe "b"
  pure value

main :: Effect Unit
main = do
  lastComponentIsRun
  lastPureIsUnwrapped
