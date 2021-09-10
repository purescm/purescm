(library
  (PureScheme.Test.Binder.Constructor lib)
  (export Qux foo)
  (import (rnrs))


  (define Qux (lambda (value0) (cons (quote Qux) (vector value0))))

  (define
    foo
    (lambda
      (v)
      (cond
        ((and (eq? (car v) (quote Qux)) (= (vector-ref (cdr v) 0) 1)) (Qux 2))
        ((eq? (car v) (quote Qux)) (Qux (vector-ref (cdr v) 0)))
        ((eq? (car v) (quote Qux)) (Qux 3))
        (else (error #f "Failed pattern match")))))
  )