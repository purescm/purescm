{ name = "purs-backend-chez"
, dependencies =
  [ "aff"
  , "argonaut"
  , "arrays"
  , "backend-optimizer"
  , "bifunctors"
  , "console"
  , "control"
  , "debug"
  , "dodo-printer"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "profunctor"
  , "safe-coerce"
  , "strings"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
