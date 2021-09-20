{ name = "my-project"
, dependencies = [ "prelude", "psci-support", "console", "minibench" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purescm"
}
