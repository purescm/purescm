(library
  (PureScheme.Test.Import.Foo lib)
  (export foo)
  (import (prefix (rnrs) scm:))


  (scm:define foo 1)
  )