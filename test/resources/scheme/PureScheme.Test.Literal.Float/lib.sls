(library
  (PureScheme.Test.Literal.Float lib)
  (export foo)
  (import (prefix (rnrs) scm:))


  (scm:define foo 23.42)
  )