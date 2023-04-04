module Snapshot.Literals.Record where

recordAccess :: forall a r. { fooBarBaz :: a | r } -> a
recordAccess = _.fooBarBaz
