{ name = "snapshots"
, dependencies = [ "assert", "console", "effect", "partial", "prelude" ]
, packages = ./packages.dhall
, sources = [ "snapshots-input/**/*.purs" ]
}
