(library
  (PureScheme.Test.Literal.Integer lib)
  (export foo)
  (import (prefix (rnrs) scm:))


  (scm:define foo 23)
  )