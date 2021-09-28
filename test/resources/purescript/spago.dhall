{ name = "my-project"
, dependencies = [ "console", "effect", "minibench", "prelude", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purescm"
}
