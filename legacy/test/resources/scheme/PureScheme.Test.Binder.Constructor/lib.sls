(library
  (PureScheme.Test.Binder.Constructor lib)
  (export Qux foo)
  (import (prefix (rnrs) scm:))


  (scm:define
    Qux
    (scm:lambda (value0) (scm:cons (scm:quote Qux) (scm:vector value0))))

  (scm:define
    foo
    (scm:lambda
      (v)
      (scm:cond
        ((scm:and
            (scm:eq? (scm:car v) (scm:quote Qux))
            (scm:= (scm:vector-ref (scm:cdr v) 0) 1))
          (Qux 2))
        ((scm:eq? (scm:car v) (scm:quote Qux))
          (Qux (scm:vector-ref (scm:cdr v) 0)))
        ((scm:eq? (scm:car v) (scm:quote Qux)) (Qux 3))
        (scm:else (scm:error #f "Failed pattern match")))))
  )