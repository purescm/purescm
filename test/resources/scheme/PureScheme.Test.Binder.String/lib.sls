(library
  (PureScheme.Test.Binder.String lib)
  (export match)
  (import (prefix (rnrs) scm:))


  (scm:define
    match
    (scm:lambda
      (v)
      (scm:cond
        ((scm:string=? v "foo") 0)
        ((scm:string=? v "bar") 1)
        (#t 2)
        (scm:else (scm:error #f "Failed pattern match")))))
  )