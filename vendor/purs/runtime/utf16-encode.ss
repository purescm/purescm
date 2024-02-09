(library (purs runtime utf16-encode)
  (export encode-utf16)
  (import (chezscheme))

  ; Encode a scheme string as an immobile bytevector
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

  )
