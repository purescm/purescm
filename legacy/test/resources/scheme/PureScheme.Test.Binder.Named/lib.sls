(library
  (PureScheme.Test.Binder.Named lib)
  (export Bar Baz match)
  (import (prefix (rnrs) scm:))


  (scm:define
    Bar
    (scm:lambda (value0) (scm:cons (scm:quote Bar) (scm:vector value0))))

  (scm:define Baz (scm:cons (scm:quote Baz) (scm:vector)))

  (scm:define
    match
    (scm:lambda
      (v)
      (scm:cond
        ((scm:eq? (scm:car v) (scm:quote Bar)) v)
        ((scm:eq? (scm:car v) (scm:quote Baz)) Baz)
        (scm:else (scm:error #f "Failed pattern match")))))
  )