{ name = "snapshots"
, dependencies =
  [ "prelude"
  ]
, packages = ../packages.dhall
, sources = [ "snapshots-input/**/*.purs" ]
}
