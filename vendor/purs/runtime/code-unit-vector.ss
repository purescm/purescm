#!chezscheme
; A UTF-16 encoded block of bytes that uses a either a bytevector or a manually
; managed block of memory (ftype-pointer). The bytevector representation is used
; for allocating string literals (using bytevector literals) while the ftype-pointer
; representation is needed for interop with native libraries like PCRE2 that work
; with pointers.
(library (purs runtime code-unit-vector)
  (export code-unit-length
          empty-code-unit-vector
          code-unit-vector-alloc
          code-unit-vector-alloc-ftype
          code-unit-vector-length
          code-unit-vector-copy!
          code-unit-vector-ref
          code-unit-vector-&ref
          code-unit-vector-set!
          code-unit-vector-copy-ftype
          code-unit-vector-pointer?
          bytevector->code-unit-vector
          string->code-unit-vector
          code-unit-vector->string)
  (import (chezscheme)
          (only (purs runtime finalizers) finalizer))

  (define code-unit-length 2)

  ; ftype-pointer and length in bytes
  (define-structure
    (code-unit-vector bv pointer length))

  ;
  ; Constructors
  ; 

  (define empty-code-unit-vector
    (make-code-unit-vector #f (make-ftype-pointer unsigned-16 0) 0))

  (define (bytevector->code-unit-vector bv)
    (make-code-unit-vector bv #f (fx/ (bytevector-length bv) code-unit-length)))

  (define (code-unit-vector-alloc n)
    (make-code-unit-vector (make-bytevector (fx* n code-unit-length)) #f n))

  (define (code-unit-vector-alloc-ftype n)
    (make-code-unit-vector #f (ftype-pointer-alloc n) n))

  (define (ftype-pointer-alloc n)
    (if (fx=? n 0)
      (make-ftype-pointer unsigned-16 0)
      (let ([pointer (make-ftype-pointer unsigned-16 (foreign-alloc (fx* (fx1+ n) (foreign-sizeof 'unsigned-16))))])
        (ftype-set! unsigned-16 () pointer n #x0)
        (finalizer pointer (lambda (p) (foreign-free (ftype-pointer-address p)))))))

  (define (code-unit-vector-ref vec i)
    (let ([p (code-unit-vector-pointer vec)])
      (if p
        (ftype-ref unsigned-16 () p i)
        (bytevector-u16-native-ref (code-unit-vector-bv vec) (fx* i code-unit-length)))))

  (define (code-unit-vector-set! vec i val)
    (let ([p (code-unit-vector-pointer vec)])
      (if p
        (ftype-set! unsigned-16 () p i val)
        (bytevector-u16-native-set! (code-unit-vector-bv vec) (fx* i code-unit-length) val))))

  (define (code-unit-vector-&ref vec i)
    (let ([p (code-unit-vector-pointer vec)])
      (if p
        (ftype-&ref unsigned-16 () p i)
        (raise-continuable
          (make-message-condition
            "Trying to take a pointer ref of a non-ftype code unit vector")))))

  (define (code-unit-vector-pointer? vec)
    (if (code-unit-vector-pointer vec) #t #f))

  ; reallocate a slice outside the scheme heap
  (define (code-unit-vector-copy-ftype vec offset len)
    (let ([p (ftype-pointer-alloc (code-unit-vector-length vec))])
      (let loop ([i 0])
        (when (fx<? i len)
          (begin
            (ftype-set! unsigned-16 ()
              p
              i
              (code-unit-vector-ref vec (fx+ offset i)))
            (loop (fx1+ i)))))
      (make-code-unit-vector #f p len)))

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
