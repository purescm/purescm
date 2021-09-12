(library
  (PureScheme.Test.Literal.IntegerArray lib)
  (export foo)
  (import (prefix (rnrs) scm:))


  (scm:define foo (scm:vector 23 42))
  )