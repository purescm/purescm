{ name = "my-project"
, dependencies = [ "prelude", "psci-support", "console" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purescm"
}
