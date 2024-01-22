#!r6rs
#!chezscheme
(library (purs runtime finalizers)
  (export finalizer
          run-finalizers)
  (import (chezscheme))

  (define finalizer-guardian (make-guardian))

  (define (finalizer o proc)
    (finalizer-guardian o (cons o proc))
    o)

  (define run-finalizers
    (lambda ()
      (let recur ()
        (let ([o (finalizer-guardian)])
          (when o
            ((cdr o) (car o))
            (recur))))))

  )
