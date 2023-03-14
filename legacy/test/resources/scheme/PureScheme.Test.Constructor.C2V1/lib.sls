(library
  (PureScheme.Test.Constructor.C2V1 lib)
  (export Bar Baz)
  (import (prefix (rnrs) scm:))


  (scm:define
    Bar
    (scm:lambda (value0) (scm:cons (scm:quote Bar) (scm:vector value0))))

  (scm:define
    Baz
    (scm:lambda (value0) (scm:cons (scm:quote Baz) (scm:vector value0))))
  )