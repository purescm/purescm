(library
  (PureScheme.Test.Binder.Vector lib)
  (export foo)
  (import (prefix (rnrs) scm:))


  (scm:define
    foo
    (scm:lambda
      (dictPartial)
      (scm:lambda
        (v)
        (scm:cond
          ((scm:and
              (scm:= (scm:vector-length v) 3)
              (scm:= (scm:vector-ref v 0) 1)
              (scm:= (scm:vector-ref v 1) 2)
              (scm:= (scm:vector-ref v 2) 3))
            4)
          ((scm:and
              (scm:= (scm:vector-length v) 2)
              (scm:= (scm:vector-ref v 0) 5)
              (scm:= (scm:vector-ref v 1) 6))
            7)
          ((scm:= (scm:vector-length v) 0) 8)
          (scm:else
            (scm:error #f "Failed pattern match CEIQUGGHMYISETVHPWRFYCFA"))))))
  )