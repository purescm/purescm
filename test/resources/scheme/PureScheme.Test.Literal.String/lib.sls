(library
  (PureScheme.Test.Literal.String lib)
  (export foo)
  (import (prefix (rnrs) scm:))


  (scm:define foo "foo")
  )