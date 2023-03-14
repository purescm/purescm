(library
  (PureScheme.Test.Constructor.Nested lib)
  (export Foo Bar)
  (import (prefix (rnrs) scm:))


  (scm:define
    Foo
    (scm:lambda (value0) (scm:cons (scm:quote Foo) (scm:vector value0))))

  (scm:define
    Bar
    (scm:lambda (value0) (scm:cons (scm:quote Bar) (scm:vector value0))))
  )