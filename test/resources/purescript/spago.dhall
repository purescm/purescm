{ name = "my-project"
, dependencies = [ "prelude", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
