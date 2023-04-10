module Snapshot.If where

import Prelude

import Effect (Effect)
import Effect.Console (log)

ifThen :: Boolean -> Int
ifThen = if _ then 0 else 1

performWhen :: Boolean -> Effect Unit
performWhen condition = when condition $ log "performWhen"

data T = T Int | U Int

t :: Partial => T -> Int
t = case _ of
  T a -> a
