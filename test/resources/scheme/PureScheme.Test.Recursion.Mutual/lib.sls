(library
  (PureScheme.Test.Recursion.Mutual lib)
  (export foo bar)
  (import (prefix (rnrs) scm:))


  (scm:define
    foo
    (scm:lambda
      (v)
      (scm:lambda
        (v1)
        (scm:cond
          ((scm:and (scm:= v 0) #t) ((foo 1) v1))
          ((scm:and (scm:= v 1) #t) ((bar 2) v1))
          ((scm:and (scm:= v 2) #t) v1)
          ((scm:and #t #t) 0)
          (scm:else (scm:error #f "Failed pattern match"))))))

  (scm:define
    bar
    (scm:lambda
      (v)
      (scm:lambda
        (v1)
        (scm:cond
          ((scm:and (scm:= v 0) #t) ((bar 1) v1))
          ((scm:and (scm:= v 1) #t) ((foo 2) v1))
          ((scm:and (scm:= v 2) #t) v1)
          ((scm:and #t #t) 0)
          (scm:else (scm:error #f "Failed pattern match"))))))
  )