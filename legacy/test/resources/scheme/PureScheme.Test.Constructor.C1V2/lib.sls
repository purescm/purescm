(library
  (PureScheme.Test.Constructor.C1V2 lib)
  (export Bar Baz)
  (import (prefix (rnrs) scm:))


  (scm:define Bar (scm:cons (scm:quote Bar) (scm:vector)))

  (scm:define Baz (scm:cons (scm:quote Baz) (scm:vector)))
  )