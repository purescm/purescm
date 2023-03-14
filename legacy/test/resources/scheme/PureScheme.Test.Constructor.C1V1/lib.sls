(library
  (PureScheme.Test.Constructor.C1V1 lib)
  (export Bar)
  (import (prefix (rnrs) scm:))


  (scm:define
    Bar
    (scm:lambda (value0) (scm:cons (scm:quote Bar) (scm:vector value0))))
  )