module PureScheme.Test.Import.Main where

import Prelude

import PureScheme.Test.Import.Foo
import PureScheme.Test.Import.Bar as Bar
import PureScheme.Test.Import.Baz (baz)

main = foo + Bar.bar + baz
