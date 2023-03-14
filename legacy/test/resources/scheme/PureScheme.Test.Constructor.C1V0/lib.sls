(library
  (PureScheme.Test.Constructor.C1V0 lib)
  (export Foo)
  (import (prefix (rnrs) scm:))


  (scm:define Foo (scm:cons (scm:quote Foo) (scm:vector)))
  )