(library
  (PureScheme.Test.Literal.StringArray lib)
  (export foo)
  (import (prefix (rnrs) scm:))


  (scm:define foo (scm:vector "foo" "bar"))
  )