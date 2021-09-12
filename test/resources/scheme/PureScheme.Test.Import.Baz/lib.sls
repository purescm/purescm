(library
  (PureScheme.Test.Import.Baz lib)
  (export baz)
  (import (prefix (rnrs) scm:))


  (scm:define baz 3)
  )