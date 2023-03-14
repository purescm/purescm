(library
  (PureScheme.Test.Import.Bar lib)
  (export bar)
  (import (prefix (rnrs) scm:))


  (scm:define bar 2)
  )