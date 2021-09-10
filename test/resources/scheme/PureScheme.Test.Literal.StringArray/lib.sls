(library
  (PureScheme.Test.Literal.StringArray lib)
  (export foo)
  (import (rnrs))


  (define foo (vector "foo" "bar"))
  )