(library
  (PureScheme.Test.Binder.String lib)
  (export match)
  (import (rnrs))


  (define
    match
    (lambda
      (v)
      (cond
        ((string=? v "foo") 0)
        ((string=? v "bar") 1)
        (#t 2)
        (else (error #f "Failed pattern match")))))
  )