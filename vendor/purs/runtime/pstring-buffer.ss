#!chezscheme
(library (purs runtime pstring-buffer)
  (export code-unit-length
          code-unit-vector-alloc
          code-unit-vector-ref
          code-unit-vector-set!
          code-unit-vector-&ref
          code-unit-vector-copy!
          encode-utf16
          decode-utf16)
  (import (chezscheme))

  (define code-unit-length 2)

  (define (code-unit-vector-alloc n)
    (make-immobile-bytevector (fx* n code-unit-length)))

  ; Access the nth code unit of the buffer
  (define (code-unit-vector-ref bv n)
    (bytevector-u16-native-ref bv (fx* n code-unit-length)))

  (define (code-unit-vector-set! bv i val)
    (bytevector-u16-native-set! bv (fx* i code-unit-length) val))

  (define (code-unit-vector-&ref bv i)
    (ftype-&ref
      unsigned-16
      ()
      (make-ftype-pointer unsigned-16 (object->reference-address bv))
      i))

  ; Copies `n` code units to dst
  (define (code-unit-vector-copy! src-buf src-offset dst dst-offset n)
    (let loop ([i 0])
      (when (fx<? i n)
        (begin
          (code-unit-vector-set!
            dst
            (fx+ dst-offset i)
            (code-unit-vector-ref src-buf (fx+ src-offset i)))
          (loop (fx1+ i))))))

  ; Encode a scheme string as an immobile UTF-16 bytevector
  (define (encode-utf16 s)
    (let* ([sn (string-length s)]
           [buf-size (do ([si 0 (fx+ si 1)]
                          [n 0 (+ n (if (char<=? (string-ref s si) #\xffff) 1 2))])
                 ((fx= si sn) n))]
           [buf (make-immobile-bytevector (fx* buf-size 2))])
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
                (bytevector-u16-native-set! buf (fx* i 2) w1)
                (bytevector-u16-native-set! buf (fx* (fx1+ i) 2) w2)
                (loop (fx1+ ci) (fx+ i 2)))
              (begin
                (bytevector-u16-native-set! buf (fx* i 2) point)
                (loop (fx1+ ci) (fx1+ i)))))))
      buf))

  (define (decode-utf16 vec offset len)
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

