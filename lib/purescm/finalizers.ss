#!r6rs
#!chezscheme
; Finalizers can be used to free memory allocated outisde the Chez heap. A
; finalizer is a callback registered using `finalizer` that will free the
; memory. The finalizer callbacks are called automatically by the `purescm`
; runtime, so there is no need to call `run-finalizers` by the user. Finalizers
; are implemented using Chez's guardians, documented here:
; <https://cisco.github.io/ChezScheme/csug10.0/smgmt.html#./smgmt:h2>
(library (purescm finalizers)
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
