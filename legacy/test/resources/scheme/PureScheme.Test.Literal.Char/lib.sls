(library
  (PureScheme.Test.Literal.Char lib)
  (export foo)
  (import (prefix (rnrs) scm:))


  (scm:define foo #\a)
  )