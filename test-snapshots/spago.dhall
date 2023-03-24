{ name = "snapshots"
, dependencies = [ "effect", "partial", "prelude" ]
, packages = ./packages.dhall
, sources = [ "snapshots-input/**/*.purs" ]
}
