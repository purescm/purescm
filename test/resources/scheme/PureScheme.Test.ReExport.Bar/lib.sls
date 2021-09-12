(library
  (PureScheme.Test.ReExport.Bar lib)
  (export bar)
  (import (prefix (rnrs) scm:))


  (scm:define bar 2)
  )