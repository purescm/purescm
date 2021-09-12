(library
  (PureScheme.Test.Binder.String lib)
  (export match)
  (import (prefix (rnrs) scm:))


  (scm:define
    match
    (scm:lambda
      (v)
      (scm:cond
        ((scm:and (scm:string=? v "foo")) 0)
        ((scm:and (scm:string=? v "bar")) 1)
        ((scm:and #t) 2)
        (scm:else (scm:error #f "Failed pattern match")))))
  )