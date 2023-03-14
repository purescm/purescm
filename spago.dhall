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
  , "maybe"
  , "newtype"
  , "partial"
  , "prelude"
  , "profunctor"
  , "safe-coerce"
  , "strings"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
