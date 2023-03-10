{ name = "purs-backend-chez"
, dependencies = [ "backend-optimizer", "console", "effect", "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
