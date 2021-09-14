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
          ((scm:= v 0) ((foo 1) v1))
          ((scm:= v 1) ((bar 2) v1))
          ((scm:= v 2) v1)
          (#t 0)
          (scm:else (scm:error #f "Failed pattern match"))))))

  (scm:define
    bar
    (scm:lambda
      (v)
      (scm:lambda
        (v1)
        (scm:cond
          ((scm:= v 0) ((bar 1) v1))
          ((scm:= v 1) ((foo 2) v1))
          ((scm:= v 2) v1)
          (#t 0)
          (scm:else (scm:error #f "Failed pattern match"))))))
  )