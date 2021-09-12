(library
  (PureScheme.Test.Literal.Bool lib)
  (export t f)
  (import (prefix (rnrs) scm:))


  (scm:define t #t)

  (scm:define f #f)
  )