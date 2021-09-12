(library
  (PureScheme.Test.ReExport.Foo lib)
  (export foo)
  (import (prefix (rnrs) scm:))


  (scm:define foo 1)
  )