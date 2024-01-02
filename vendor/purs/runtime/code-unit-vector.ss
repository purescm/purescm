#!chezscheme

; 
; Code unit vector
; 
; Much like bytevector but with manually managed memory
;
(library (purs runtime code-unit-vector)
  (export code-unit-length
          empty-code-unit-vector
          code-unit-vector-alloc
          code-unit-vector-length
          code-unit-vector-copy!
          code-unit-vector-ref
          code-unit-vector-&ref
          code-unit-vector-set!)
  (import (chezscheme)
          (only (purs runtime finalizers) finalizer))

  (define code-unit-length 2)

  ; ftype-pointer and length in bytes
  (define-structure
    (code-unit-vector pointer length))

  (define empty-code-unit-vector
    (make-code-unit-vector #f 0))

  (define (code-unit-vector-alloc n)
    (if (fx=? n 0)
      empty-code-unit-vector
      (let ([pointer (finalizer (make-ftype-pointer unsigned-16 (foreign-alloc (fx* n code-unit-length)))
                                (lambda (p) (foreign-free (ftype-pointer-address p))))])
        (make-code-unit-vector pointer n))))

  (define (code-unit-vector-ref vec i)
    (ftype-ref unsigned-16 () (code-unit-vector-pointer vec) i))

  (define (code-unit-vector-set! vec i val)
    (ftype-set! unsigned-16 () (code-unit-vector-pointer vec) i val))

  (define (code-unit-vector-&ref vec i)
    (ftype-&ref unsigned-16 () (code-unit-vector-pointer vec) i))

  ; copies n bytes to dst
  (define (code-unit-vector-copy! src-buf src-offset dst dst-offset n)
    (let loop ([i 0])
      (when (fx<? i n)
        (begin
          (code-unit-vector-set!
            dst
            (fx+ dst-offset i)
            (code-unit-vector-ref src-buf (fx+ src-offset i)))
          (loop (fx1+ i))))))

 )
