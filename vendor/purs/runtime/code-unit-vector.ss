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
          code-unit-vector-set!
          string->code-unit-vector
          code-unit-vector->string)
  (import (chezscheme)
          (only (purs runtime finalizers) finalizer))

  (define code-unit-length 2)

  ; ftype-pointer and length in bytes
  (define-structure
    (code-unit-vector pointer length))

  (define empty-code-unit-vector
    (make-code-unit-vector (make-ftype-pointer unsigned-16 0) 0))

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

  (define (string->code-unit-vector s)
    (let* ([sn (string-length s)]
           [buf-size (do ([si 0 (fx+ si 1)]
                          [n 0 (+ n (if (char<=? (string-ref s si) #\xffff) 1 2))])
                 ((fx= si sn) n))]
           [buf (code-unit-vector-alloc buf-size)])
      (let loop ([ci 0] [i 0])
        (if (fx<? ci sn)
          (let ([point (char->integer (string-ref s ci))])
            (if (fx>? point #x10FFFF)
              (raise-continuable
                (make-message-condition
                  (format "Value ~d is too large to encode as UTF-16 code point" point))))
            (if (fx<=? #x10000 point)
              (let* ([code (fx- point #x10000)]
                     [w1 (fxlogor #xD800 (fxsrl code 10))]
                     [w2 (fxlogor #xDC00 (fxlogand code #x3FF))])
                (code-unit-vector-set! buf i w1)
                (code-unit-vector-set! buf (fx1+ i) w2)
                (loop (fx1+ ci) (fx+ i 2)))
              (begin
                (code-unit-vector-set! buf i point)
                (loop (fx1+ ci) (fx1+ i)))))))
      buf))

  (define (code-unit-vector->string vec offset len)
    ; First allocate a string based on the code unit size
    ; and then truncate at the end.
    (let* ([out (make-string len)])
      (let loop ([i 0] [char-i 0])
        (if (fx<? i len)
          (let* ([w1 (code-unit-vector-ref vec (fx+ offset i))])
            (cond
              ;; Two-word encoding? Check for high surrogate
              [(and (fx<= #xD800 w1 #xDBFF) (fx>=? (fx- len i) 2))
               (let ([w2 (code-unit-vector-ref vec (fx+ offset i 1))])
                 (if (fx<= #xDC00 w2 #xDFFF)
                   (begin
                     (string-set! out
                                  char-i
                                  (integer->char
                                    (fx+
                                      (fxlogor
                                        (fxsll (fx- w1 #xD800) 10)
                                        (fx- w2 #xDC00))
                                      #x10000)))
                     (loop (fx+ i 2) (fx1+ char-i)))
                   (begin
                     (string-set! out char-i #\xfffd)
                     (loop (fx+ i 1) (fx1+ char-i)))))]
              ;; misplaced continuation word?
              [(fx<= #xDC00 w1 #xDFFF)
               (begin
                 (string-set! out char-i #\xfffd)
                 (loop (fx+ i 1) (fx1+ char-i)))]
              ;; one-word encoding
              [else (begin (string-set! out char-i (integer->char w1)) (loop (fx+ i 1) (fx1+ char-i)))]))
          (begin (string-truncate! out char-i) out)))))
 )
