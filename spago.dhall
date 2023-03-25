{ name = "purs-backend-chez"
, dependencies =
  [ "aff"
  , "argonaut"
  , "arrays"
  , "backend-optimizer"
  , "console"
  , "control"
  , "dodo-printer"
  , "effect"
  , "foldable-traversable"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "prelude"
  , "safe-coerce"
  , "strings"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
