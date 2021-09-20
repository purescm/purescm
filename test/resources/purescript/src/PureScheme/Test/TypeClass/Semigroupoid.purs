module PureScheme.Test.TypeClass.Semigroupoid where

class Semigroupoid :: forall k. (k -> k -> Type) -> Constraint
class Semigroupoid a where
  compose :: forall b c d. a c d -> a b c -> a b d

instance semigroupoidFn :: Semigroupoid (->) where
  compose f g x = f (g x)
