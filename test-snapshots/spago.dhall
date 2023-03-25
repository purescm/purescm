{ name = "snapshots"
, dependencies = [ "partial", "prelude" ]
, packages = ./packages.dhall
, sources = [ "snapshots-input/**/*.purs" ]
}
