#!chezscheme
(library (purs runtime pstring)
  (export pstring-ref
          pstring-length
          pstring-uncons-code-unit
          pstring-uncons-code-point
          (rename (make-pstring-of-length make-pstring))
          pstring
          pstring-empty?
          (rename ($pstring? pstring?))
          pstring=?
          ; pstring-hash
          pstring-slice
          pstring-trim
          pstring-take
          pstring-drop
          pstring-index-of
          pstring-last-index-of
          pstring-singleton
          pstring-join-with
          pstring->string
          pstring->number
          number->pstring
          pstring->symbol
          string->pstring
          pstring-ci=?
          pstring-concat
          char-flexvector->pstring
          pstring->char-flexvector

          pstring-replace
          pstring-replace-all
          pstring-split

          ;; code points
          pstring-ref-code-point
          pstring-length-code-points
          pstring->list
          code-points->pstring
          pstring<?
          pstring>?
          pstring<=?
          pstring>=?
          pstring-foldcase
          pstring-downcase
          pstring-upcase
          pstring-take-code-points

          pstring-make-regex
          regex-source
          regex-flags
          pstring-regex-match
          pstring-regex-search
          pstring-regex-replace
          pstring-regex-replace-by)
  (import (chezscheme)
          (only (purs runtime finalizers) finalizer)
          (purs runtime code-unit-vector)
          (prefix (purs runtime srfi :214) srfi:214:))

  (define-structure
    ($pstring buffer length))

  (define (make-pstring buf offset len)
    (make-$pstring (cons buf offset) len))

  (define (pstring-buffer bs)
    (car ($pstring-buffer bs)))

  (define (pstring-offset bs)
    (cdr ($pstring-buffer bs)))

  (define pstring-length $pstring-length)

  (define empty-pstring (make-pstring empty-code-unit-vector 0 0))

  (define (pstring-empty? bs)
    (fx=? (pstring-length bs) 0))

  ;; Do x and y point to the same object in memory?
  (define (pstring-eq? x y)
    (and (fx=? (pstring-length x) (pstring-length y))
               (fx=? (pstring-offset x) (pstring-offset y))
               (eq? (pstring-buffer x) (pstring-buffer y))))

  ;; this is our version of `make-string`
  (define (make-pstring-of-length n)
    (make-pstring (code-unit-vector-alloc n) 0 n))

  ;; NOTE: this only takes in PS chars which are guaranteed to
  ;; be only one word in size (one code unit)
  (define (pstring-singleton c)
    (let ([bv (code-unit-vector-alloc 1)])
      (code-unit-vector-set! bv 0 (char->integer c))
      (make-pstring bv 0 1)))

  (define (pstring=? x y)
    ;; Assumes the buffers have the same length
    (define (pstring-equal-code-units? x y)
      (let loop ([n 0] [tailx x] [taily y])
        (or
          (or (pstring-empty? tailx) (pstring-empty? taily))
          (let-values ([(hx tx) ($pstring-uncons-code-unit tailx)]
                       [(hy ty) ($pstring-uncons-code-unit taily)])
            (and (fx=? hx hy) (loop (fx1+ n) tx ty))))))

    (if (fx=? (pstring-length x) (pstring-length y))
      (cond
        ;; Do they point to the same object in memory?
        [(and (fx=? (pstring-offset x) (pstring-offset y))
              (eq? (pstring-buffer x) (pstring-buffer y))) #t]
        [(pstring-equal-code-units? x y) #t]
        [else #f])
      #f))

  ; ;; Mostly taken from Chez Scheme `newhash.ss` and updated to work with `pstring`
  ; (define (pstring-hash bs)
  ;   (define (hcabs hc) (if (fx< hc 0) (fxnot hc) hc))
  ;   (define (update hc k)
  ;     (let ([hc2 (fx+/wraparound hc (fxsll/wraparound (fx+/wraparound hc k) 10))])
  ;       (fxlogxor hc2 (fxsrl hc2 6))))
  ;   (define (bvupdate hc bv i)
  ;     (update hc (bytevector-u8-ref bv i)))
  ;   (let ([n (pstring-length bs)]
  ;         [offset (pstring-offset bs)]
  ;         [bv (pstring-buffer bs)])
  ;     (if (fx<= n 16)
  ;         (do ([i 0 (fx+ i 1)] [hc 440697712 (bvupdate hc bv (fx+ i offset))])
  ;           ((fx= i n) (hcabs hc)))
  ;         (do ([i 0 (fx+ i 1)]
  ;              [hc 440697712 (bvupdate hc bv (fx+ i offset))])
  ;             ((fx= i 5)
  ;              (do ([i (fx- n 5) (fx+ i 1)]
  ;                   [hc hc (bvupdate hc bv (fx+ i offset))])
  ;                  ((fx= i n)
  ;                   (let ([stride (fxsrl n 4)])
  ;                     (do ([i 5 (fx+ i stride)]
  ;                          [hc hc (bvupdate hc bv (fx+ i offset))])
  ;                         ((fx>= i n) (hcabs hc)))))))))))

  (define-syntax string->pstring
    (lambda (x)
      (syntax-case x ()
        [(string->pstring s)
         (let ([d (syntax->datum #'s)])
           (if (string? d)
             #`(let ([bv #,(string->utf16 d (native-endianness))])
                 (make-pstring (bytevector->code-unit-vector bv) 0 (fx/ (bytevector-length bv) 2)))
             #'(let ([bv (string->utf16 s (native-endianness))])
                 (make-pstring (bytevector->code-unit-vector bv) 0 (fx/ (bytevector-length bv) 2)))))])))

  ; (define (string->pstring s)
  ;   (let ([buf (string->code-unit-vector s)])
  ;     (make-pstring buf 0 (code-unit-vector-length buf))))

  (define (pstring->string bs)
    (code-unit-vector->string (pstring-buffer bs) (pstring-offset bs) (pstring-length bs)))

  (define (pstring-read-code-unit bs)
    (code-unit-vector-ref (pstring-buffer bs) (pstring-offset bs)))

  (define (pstring-read-word-end bs)
    (code-unit-vector-ref (pstring-buffer bs) (fx- (fx+ (pstring-offset bs) (pstring-length bs)) 1)))

  (define (pstring-forward-code-units bs n)
    (make-pstring
      (pstring-buffer bs)
      (fx+ (pstring-offset bs) n)
      (fx- (pstring-length bs) n)))

  (define (pstring-drop-code-units bs n)
    (make-pstring
      (pstring-buffer bs)
      (pstring-offset bs)
      (fx- (pstring-length bs) n)))

  (define ($pstring-uncons-code-unit bs)
    (if (pstring-empty? bs)
      (raise-continuable
        (make-message-condition
          (format "$pstring-uncons-code-unit: cannot uncons an empty pstring ~a" bs)))
      (let ([w1 (pstring-read-code-unit bs)]
            [tail (pstring-forward-code-units bs 1)])
        (values w1 tail))))

  (define (pstring-uncons-code-unit bs)
    (let-values ([(head tail) ($pstring-uncons-code-unit bs)])
      (values (integer->char head) tail)))

  ;; Constant-time ref, like string-ref.
  ;; Returns a scheme `char`.
  (define (pstring-ref bs n)
    (define (pstring-ref-code-unit bs n)
      (let ([bv (pstring-buffer bs)])
        (if (fx<? n (pstring-length bs))
          (code-unit-vector-ref bv (fx+ n (pstring-offset bs)))
          ;; not enough bytes to read a full code unit
          (raise-continuable
            (make-message-condition
              (format "pstring-ref-code-unit ~d is not a valid index" n))))))

    (integer->char (pstring-ref-code-unit bs n)))

  (define pstring-slice
    (case-lambda
      [(bs start)
       (pstring-slice bs start (pstring-length bs))]
      [(bs start end)
        (let* ([start-index (fxmin (fxmax 0 start) (pstring-length bs))]
               [end-index (fxmin (fxmax 0 end) (pstring-length bs))]
               [len (fx- end-index start-index)])
          (if (fx<? len 0)
            empty-pstring
            (make-pstring
              (pstring-buffer bs)
              (fx+ (pstring-offset bs) start-index)
              len)))]))

  (define (pstring-take bs n)
    (pstring-slice bs 0 n))

  (define (pstring-drop bs n)
    (pstring-slice bs n))

  (define (pstring-trim bs)
    (define (whitespace? c)
      (or
          ; Whitespace characters
          (fx=? c #x0009)
          (fx=? c #x000B)
          (fx=? c #x000C)
          (fx=? c #x0020)
          (fx=? c #x00A0)
          (fx=? c #xFEFF)

          ; Line terminators
          (fx=? c #x000A)
          (fx=? c #x000D)
          (fx=? c #x2028)
          (fx=? c #x2029)))

    (let ([preceding
            (let loop ([rest bs])
              (if (pstring-empty? rest)
                rest
                (let-values ([(head tail) ($pstring-uncons-code-unit rest)])
                  (if (whitespace? head)
                    (loop tail)
                    rest))))])
      (let loop ([rest preceding])
        (if (pstring-empty? rest)
          rest
          (let ([last (pstring-read-word-end rest)]
                [prefix (pstring-drop-code-units rest 1)])
            (if (whitespace? last) (loop prefix) rest))))))

  (define (pstring-index-of bs pattern)
    (if (pstring-empty? pattern)
      0
      (let loop ([i 0]
                 [candidate #f]    ; the index of the first matching char
                 [hs bs]           ; haystack
                 [demand pattern]) ; chars left to be found
        (cond
          ;; Nothing is demanded, so we are done
          [(pstring-empty? demand) candidate]
          ;; In the middle of matching but we have no more input. No match found.
          [(pstring-empty? hs) #f]
          [else
            (let-values ([(pc demand-rest) ($pstring-uncons-code-unit demand)]
                         [(ic hs-rest) ($pstring-uncons-code-unit hs)])
              (if (fx=? pc ic)
                ;; Found a match for char, advance to next char
                (loop (fx1+ i) (or candidate i) hs-rest demand-rest)
                (if candidate
                  ;; No match, rewind demand and start over at the same spot
                  (loop i #f hs pattern)
                  (loop (fx1+ i) #f hs-rest pattern))))]))))

  (define (pstring-last-index-of bs pattern)
    (if (pstring-empty? pattern)
      (pstring-length bs)
      (let loop ([i 0]
                 [last-match-candidate #f]
                 [candidate #f]    ; the index of the first matching char
                 [hs bs]           ; haystack
                 [demand pattern]) ; chars left to be found
        (cond
          [(and (not (pstring-empty? hs)) (pstring-empty? demand))
           ;; found a match but haystack not consumed, continue searching
           (loop i candidate #f hs pattern)]
          ;; Nothing is demanded, so we are done
          [(pstring-empty? demand) candidate]
          ;; In the middle of matching but we have no more input.
          [(and (pstring-empty? hs) (not (pstring-empty? demand))) last-match-candidate]
          [else
            (let-values ([(pc demand-rest) ($pstring-uncons-code-unit demand)]
                         [(ic hs-rest) ($pstring-uncons-code-unit hs)])
              (if (fx=? pc ic)
                ;; Found a match for char, advance to next char
                (loop (fx1+ i) last-match-candidate (or candidate i) hs-rest demand-rest)
                (if candidate
                  ;; No match, rewind demand and start over at the same spot
                  (loop i last-match-candidate #f hs pattern)
                  (loop (fx1+ i) last-match-candidate #f hs-rest pattern))))]))))

  (define (pstring-concat . xs)
    (let* ([len (fold-right (lambda (s a) (fx+ (pstring-length s) a)) 0 xs)]
           [buf (code-unit-vector-alloc len)])
      (let loop ([i 0] [ls xs])
        (if (pair? ls)
          (let* ([bs (car ls)]
                 [slen (pstring-length bs)])
            (code-unit-vector-copy! (pstring-buffer bs) (pstring-offset bs) buf i slen)
            (loop (fx+ i slen) (cdr ls)))
          (make-pstring buf 0 len)))))

  (define (pstring-join-with xs separator)
    (let* ([len (srfi:214:flexvector-fold (lambda (len s) (fx+ len (pstring-length s))) 0 xs)]
           [xs-count (srfi:214:flexvector-length xs)]
           [separator-count (if (fx=? xs-count 0) 0 (fx1- xs-count))]
           [separator-len (pstring-length separator)]
           [bv-len (fx+ len (fx* separator-count separator-len))]
           [bv (code-unit-vector-alloc bv-len)])
      (let loop ([i 0]
                 [bi 0])
        (if (fx<? i xs-count)
          (let* ([s (srfi:214:flexvector-ref xs i)]
                 [len (pstring-length s)])
            (if (fx>? i 0)
              (begin
                (code-unit-vector-copy!
                  (pstring-buffer separator)
                  (pstring-offset separator)
                  bv
                  (fx+ bi)
                  separator-len)
                (code-unit-vector-copy!
                  (pstring-buffer s)
                  (pstring-offset s)
                  bv
                  (fx+ bi separator-len)
                  len)
                (loop (fx1+ i) (fx+ bi len separator-len)))
              (begin
                (code-unit-vector-copy! (pstring-buffer s) (pstring-offset s) bv bi len)
                (loop (fx1+ i) (fx+ bi len)))))
          (make-pstring bv 0 bv-len)))))

  (define pstring->number
    (case-lambda
      [(bs) (string->number (pstring->string bs))]
      [(bs radix) (string->number (pstring->string bs) radix)]))

  (define number->pstring
    (case-lambda
      [(n) (string->pstring (number->string n))]
      [(n radix) (string->pstring (number->string n radix))]))

  (define (pstring->symbol bs)
    (string->symbol (pstring->string bs)))

  (define (pstring . chars)
    (let* ([len (length chars)]
           [cv (code-unit-vector-alloc len)])
      (let loop ([i 0] [rest chars])
        (if (null? rest)
          cv
          (begin
            (code-unit-vector-set! cv i (char->integer (car rest)))
            (loop (fx1+ i) (cdr rest)))))
      (make-pstring cv 0 len)))

  ;; TODO add a proper implementation
  (define (pstring-ci=? x y)
    (string-ci=? (pstring->string x) (pstring->string y)))

  (define (char-flexvector->pstring v)
    (let* ([len (srfi:214:flexvector-length v)]
           [bv (code-unit-vector-alloc len)])
      (srfi:214:flexvector-for-each/index
        (lambda (i c)
          (code-unit-vector-set! bv i (char->integer c))) 
        v)
      (make-pstring bv 0 len)))

  (define (pstring->char-flexvector bs)
    (let* ([len (pstring-length bs)]
           [fv (srfi:214:make-flexvector len)])
      (let loop ([i 0] [rest bs])
        (if (pstring-empty? rest)
          fv
          (let-values ([(c tail) (pstring-uncons-code-unit rest)])
            (srfi:214:flexvector-set! fv i c)
            (loop (fx1+ i) tail))))))

  (define (pstring-&ref bs i)
    (if (code-unit-vector-pointer? (pstring-buffer bs))
      (code-unit-vector-&ref (pstring-buffer bs) (fx+ (pstring-offset bs) i))
      (begin
        (let ([buffer (code-unit-vector-copy-ftype
                        (pstring-buffer bs)
                        (pstring-offset bs)
                        (pstring-length bs))])
          (set-$pstring-buffer! bs (cons buffer 0))
          (code-unit-vector-&ref buffer i)))))


  ;; 
  ;; Modifications
  ;;

  (define (pstring-replace bs pattern replacement)
    (if (pstring-empty? pattern)
      bs
      (let ([i (pstring-index-of bs pattern)])
        (if (not i)
          bs
          (let* ([len (fx+ (fx- (pstring-length bs) (pstring-length pattern))
                           (pstring-length replacement))]
                 [bv (code-unit-vector-alloc len)])
            (code-unit-vector-copy! (pstring-buffer bs) (pstring-offset bs) bv 0 i)
            (code-unit-vector-copy! (pstring-buffer replacement)
                              (pstring-offset replacement)
                              bv
                              i
                              (pstring-length replacement))
            (code-unit-vector-copy! (pstring-buffer bs)
                              (fx+ (pstring-offset bs) i (pstring-length pattern))
                              bv
                              (fx+ i (pstring-length replacement))
                              (fx- (pstring-length bs) i (pstring-length pattern)))
            (make-pstring bv 0 len))))))

  ; Find all occurences and return their indices as a list
  (define (all-index-of bs pattern)
    (let go ([start 0])
      (let ([slice (pstring-drop bs start)])
        (if (or (pstring-empty? bs) (pstring-empty? pattern))
          '()
          (let ([i (pstring-index-of slice pattern)])
            (if i
              (cons (fx+ start i)
                    (go (fx+ start i (pstring-length pattern))))
              '()))))))

  (define (pstring-replace-all bs pattern replacement)
    (if (pstring-empty? pattern)
      bs
      (let* ([is (all-index-of bs pattern)]
             [replacements-delta (fx* (length is)
                                      (fx- (pstring-length pattern)
                                           (pstring-length replacement)))]
             [len (fx- (pstring-length bs) replacements-delta)]
             [bv (code-unit-vector-alloc len)])
        (let loop ([bsi 0] ; where we are at bs
                   [bvi 0] ; where we are at bv
                   [rest is])
          (if (null? rest)
            ; copy the left-overs into place
            (code-unit-vector-copy! (pstring-buffer bs)
                                    (fx+ (pstring-offset bs) bsi)
                                    bv
                                    bvi
                                    (fx- (pstring-length bs) bsi))
            (let* ([i (car rest)] [before-len (fx- i bsi)])
              ;; copy stuff before the match
              (code-unit-vector-copy! (pstring-buffer bs)
                                (fx+ (pstring-offset bs) bsi)
                                bv
                                bvi
                                before-len)
              ;; the replacement itself
              (code-unit-vector-copy! (pstring-buffer replacement)
                                (pstring-offset replacement)
                                bv
                                (fx+ bvi before-len)
                                (pstring-length replacement))
              (loop
                (fx+ i (pstring-length pattern))
                (fx+ (fx+ bvi before-len) (pstring-length replacement))
                (cdr rest)))))
        (make-pstring bv 0 len))))

  (define (pstring-split bs pattern)
    (cond
      [(pstring-empty? bs) (srfi:214:flexvector)]
      [(pstring-empty? pattern)
        (let* ([len (pstring-length bs)]
               [fv (srfi:214:make-flexvector len)])
          (let loop ([i 0] [rest bs])
            (if (pstring-empty? rest)
              fv
              (let-values ([(c tail) (pstring-uncons-code-unit rest)])
                (srfi:214:flexvector-set! fv i (pstring-singleton c))
                (loop (fx1+ i) tail)))))]
      [else
        (let* ([all-indices (all-index-of bs pattern)]
               [vec (srfi:214:make-flexvector (fx1+ (length all-indices)))])
          (let loop ([indices all-indices]
                     [i 0]
                     [pi 0])
            (if (null? indices)
              (begin
                (srfi:214:flexvector-set! vec i (pstring-slice bs pi))
                vec)
              (let ([index (car indices)])
                (srfi:214:flexvector-set! vec i (pstring-slice bs pi index))
                (loop (cdr indices) (fx1+ i) (fx+ index (pstring-length pattern)))))))]))


  ;;
  ;; Code points
  ;;

  ;; Low-level unconsing
  (define (pstring-uncons-code-point bs)
    (if (pstring-empty? bs)
      (raise-continuable
        (make-message-condition "pstring-uncons-code-point: pstring is empty"))
      (let* ([buf (pstring-buffer bs)]
             [offset (pstring-offset bs)]
             [w1 (code-unit-vector-ref buf offset)])
        (cond
          ;; Two-word encoding? Check for high surrogate
          [(and (fx<= #xD800 w1 #xDBFF) (fx>=? (pstring-length bs) 2))
           (let ([w2 (code-unit-vector-ref buf (fx1+ offset))])
             ;; low surrogate?
             (if (fx<= #xDC00 w2 #xDFFF)
               (values
                 (fx+
                   (fxlogor
                     (fxsll (fx- w1 #xD800) 10)
                     (fx- w2 #xDC00))
                   #x10000)
                 (pstring-forward-code-units bs 2))
               ;; low surrogate not found, just return the high surrogate
               (values w1 (pstring-forward-code-units bs 1))))]
          ;; misplaced continuation word?
          [(fx<= #xDC00 w1 #xDFFF)
           (values w1 (pstring-forward-code-units bs 1))]
          ;; one-word encoding
          [else (values w1 (pstring-forward-code-units bs 1))]))))

  (define (pstring-ref-code-point bs n)
    (let loop ([i 0] [cur bs])
      (if (pstring-empty? cur)
        (raise-continuable
          (make-message-condition
            (format "pstring-ref-code-point: ~d is not a valid index" n))))
      (let-values ([(head tail) (pstring-uncons-code-point cur)])
        (if (fx=? i n)
          head
          (loop (fx1+ i) tail)))))

  ;; Length in code points
  (define pstring-length-code-points
    (lambda (s)
      (let loop ([i 0] [cur s])
        (if (pstring-empty? cur)
          i
          (let-values ([(_ tail) (pstring-uncons-code-point cur)])
            (loop (fx1+ i) tail))))))

  ;; linear-time, returns a list of `char`s
  (define (pstring->list bs)
    (let loop ([rest bs] [ls '()])
      (if (pstring-empty? rest)
        (reverse ls)
        (let-values ([(head tail) (pstring-uncons-code-point rest)])
          (loop tail (cons (integer->char head) ls))))))

  ;; Turns raw code point scalar values into a UTF-16 encoded pstring.
  ;; Let's you generate a pstring with invalid unicode.
  (define (code-points->pstring . xs)
    (let* ([bvs (map utf16-encode xs)]
           [len (fold-left (lambda (total bv) (fx+ total (code-unit-vector-length bv))) 0 bvs)]
           [res (code-unit-vector-alloc len)])
      (let loop ([resi 0] [rest bvs])
        (if (null? rest)
          res
          (begin
            (code-unit-vector-copy! (car rest) 0 res resi (code-unit-vector-length (car rest)))
            (loop (fx+ resi (code-unit-vector-length (car rest))) (cdr rest)))))
      (make-pstring res 0 len)))

  (define (pstring<? x y)
    (and
      (not (pstring-eq? x y))
      (let loop ([tailx x] [taily y])
        (or
          (and (pstring-empty? tailx)
               (not (pstring-empty? taily)))
          (and
            (not (pstring-empty? tailx))
            (not (pstring-empty? taily))
            (let-values ([(hx tx) (pstring-uncons-code-point tailx)]
                         [(hy ty) (pstring-uncons-code-point taily)])
              (let ([c1 (integer->char hx)]
                    [c2 (integer->char hy)])
                (or (char<? c1 c2)
                    (and (char=? c1 c2) (loop tx ty))))))))))

  (define (pstring>? x y)
    (and
      (not (pstring-eq? x y))
      (let loop ([tailx x] [taily y])
        (or
          ; is x longer than y?
          (and (not (pstring-empty? tailx))
               (pstring-empty? taily))
          (and
            (not (pstring-empty? tailx))
            (not (pstring-empty? taily))
            (let-values ([(hx tx) (pstring-uncons-code-point tailx)]
                         [(hy ty) (pstring-uncons-code-point taily)])
              (let ([c1 (integer->char hx)]
                    [c2 (integer->char hy)])
                (or (char>? c1 c2)
                    (and (char=? c1 c2) (loop tx ty))))))))))

  (define (pstring<=? x y)
    (or
      (pstring-eq? x y)
      (let loop ([tailx x] [taily y])
        (or
          (and
            (pstring-empty? tailx)
            (pstring-empty? taily))
          (and (pstring-empty? tailx)
               (not (pstring-empty? taily)))
          (and
            (not (pstring-empty? tailx))
            (not (pstring-empty? taily))
            (let-values ([(hx tx) (pstring-uncons-code-point tailx)]
                         [(hy ty) (pstring-uncons-code-point taily)])
              (let ([c1 (integer->char hx)]
                    [c2 (integer->char hy)])
                (or (char<? c1 c2)
                    (and (char=? c1 c2) (loop tx ty))))))))))

  (define (pstring>=? x y)
    (or
      (pstring-eq? x y)
      (let loop ([tailx x] [taily y])
        (or
          (and
            (pstring-empty? tailx)
            (pstring-empty? taily))
          (and (not (pstring-empty? tailx))
               (pstring-empty? taily))
          (and
            (not (pstring-empty? tailx))
            (not (pstring-empty? taily))
            (let-values ([(hx tx) (pstring-uncons-code-point tailx)]
                         [(hy ty) (pstring-uncons-code-point taily)])
              (let ([c1 (integer->char hx)]
                    [c2 (integer->char hy)])
                (or (char>? c1 c2)
                    (and (char=? c1 c2) (loop tx ty))))))))))

  (define (pstring-foldcase bs)
    (string->pstring (string-foldcase (pstring->string bs))))

  (define (pstring-downcase bs)
    (string->pstring (string-downcase (pstring->string bs))))

  (define (pstring-upcase bs)
    (string->pstring (string-upcase (pstring->string bs))))

  (define (pstring-take-code-points bs n)
    (if (fx<? n 1)
      empty-pstring
      (let loop ([i n] [tail bs])
        (cond
          [(and (fx>? i 0) (fx=? (pstring-length tail) 0)) bs]
          [(fx=? i 0) (pstring-take bs (fx- (pstring-length bs) (pstring-length tail)))]
          [else
            (let-values ([(_ t) (pstring-uncons-code-point tail)])
              (loop (fx1- i) t))]))))

  ;;
  ;; Low level codec
  ;;

  (define (utf16-encode point)
    (if (fx>? point #x10FFFF)
      (raise-continuable
        (make-message-condition
          (format "Value ~d is too large to encode as UTF-16 code point" point))))
    (if (fx<=? #x10000 point)
      (let* ([bv (code-unit-vector-alloc 2)]
             [code (fx- point #x10000)]
             [w1 (fxlogor #xD800 (fxsrl code 10))]
             [w2 (fxlogor #xDC00 (fxlogand code #x3FF))])
        (code-unit-vector-set! bv 0 w1)
        (code-unit-vector-set! bv 1 w2)
        bv)
      (let ([bv (code-unit-vector-alloc 1)])
        (code-unit-vector-set! bv 0 point)
        bv)))


  ;; 
  ;; Regex
  ;;

  (define-structure
    (regex code match-data source flags))

  (define (pstring-regex-replace-by regex subject f)
    (if (regex-has-flag regex PCRE2_SUBSTITUTE_GLOBAL)
      (pstring-regex-replace-all regex subject f)
      (pstring-regex-replace-single regex subject f)))

  (define identity (lambda (x) x))

  (define (pstring-regex-replace-all regex bs f)
    (let*-values ([(delta all-matches)
                    (let match-next ([sub-bs bs] [delta 0] [all-matches-reverse '()])
                      (let ([matches (pstring-regex-match regex sub-bs identity #f)])
                        (if (and matches (fx>? (srfi:214:flexvector-length matches) 0))
                          (let* ([match (srfi:214:flexvector-ref matches 0)]
                                 [_ (srfi:214:flexvector-remove-front! matches)]
                                 [replacement (f match matches)])
                            (match-next
                              ; Should slice be used here?
                              (make-pstring
                                (pstring-buffer sub-bs)
                                (fx+ (pstring-offset match) (pstring-length match))
                                (fx- (pstring-length sub-bs)
                                     (fx- (fx+ (pstring-offset match) (pstring-length match))
                                          (pstring-offset sub-bs))))
                              (fx+ delta (fx- (pstring-length replacement) (pstring-length match)))
                              (cons (cons match replacement) all-matches-reverse)))
                          (values delta (reverse all-matches-reverse)))))]
                  [(len) (fx+ (pstring-length bs) delta)]
                  [(bv) (code-unit-vector-alloc len)])
      (let loop ([bsi (pstring-offset bs)] [bvi 0] [rest all-matches])
        (if (null? rest)
          ; copy the left-overs into place
          (code-unit-vector-copy! (pstring-buffer bs)
                                bsi
                                bv
                                bvi
                                (fx- (pstring-length bs) (fx- bsi (pstring-offset bs))))
          (let* ([match (caar rest)]
                 [replacement (cdr (car rest))]
                 [i (pstring-offset match)]
                 [before-len (fx- i bsi)])
            ;; copy stuff before the match
            (code-unit-vector-copy! (pstring-buffer bs) bsi bv bvi before-len)
            ;; the replacement itself
            (code-unit-vector-copy! (pstring-buffer replacement)
                              (pstring-offset replacement)
                              bv
                              (fx+ bvi before-len)
                              (pstring-length replacement))
            (loop
              (fx+ (pstring-offset match) (pstring-length match))
              (fx+ (fx+ bvi before-len) (pstring-length replacement))
              (cdr rest)))))

          (make-pstring bv 0 len)))

  (define (pstring-regex-replace-single regex bs f)
    (let ([matches (pstring-regex-match regex bs identity #f)])
      (if (and matches (fx>? (srfi:214:flexvector-length matches) 0))
        (let* ([match (srfi:214:flexvector-ref matches 0)]
               [_ (srfi:214:flexvector-remove-front! matches)]
               [replacement (f match matches)]
               [delta (fx- (pstring-length replacement) (pstring-length match))]
               [len (fx+ (pstring-length bs) delta)]
               [buf (code-unit-vector-alloc len)])
          (let* ([before-len (fx- (pstring-offset match) (pstring-offset bs))])
            ;; copy stuff before the match
            (code-unit-vector-copy!
              (pstring-buffer bs)
              (pstring-offset bs)
              buf
              0
              before-len)
            ;; the replacement itself
            (code-unit-vector-copy!
              (pstring-buffer replacement)
              (pstring-offset replacement)
              buf
              before-len
              (pstring-length replacement))
            ; copy the stuff after the match
            (code-unit-vector-copy!
              (pstring-buffer bs)
              (fx+ (pstring-offset match) (pstring-length match))
              buf
              (fx+ before-len (pstring-length replacement))
              (fx- (pstring-length bs)
                   (fx+ before-len (pstring-length match))))
            (make-pstring buf 0 len)))
        bs)))

  (define (pstring-regex-replace regex subject replacement)
    (let* ([match-data (pcre2_match_data_create_from_pattern_16 (regex-code regex) 0)]
           [buf-len (make-ftype-pointer size_t (foreign-alloc (foreign-sizeof 'size_t)))]
           [subject-addr (pstring-&ref subject 0)]
           [subject-len (pstring-length subject)]
           [replacement-addr (pstring-&ref replacement 0)]
           [replacement-len (pstring-length replacement)]
           [start-offset 0]
           [match-context 0]
           ; first calculate the size of the output buffer by passing in 0 as the buf size
           ; and using PCRE2_SUBSTITUTE_OVERFLOW_LENGTH
           [_ (ftype-set! size_t () buf-len 0 0)]
           [res (pcre2_substitute_16
                (regex-code regex)
                subject-addr
                subject-len
                start-offset
                (fxlogor (fxlogand (regex-flags regex) PCRE2_SUBSTITUTE_GLOBAL)
                         PCRE2_SUBSTITUTE_OVERFLOW_LENGTH)
                match-data
                match-context
                replacement-addr
                replacement-len
                ; basically a null pointer
                (make-ftype-pointer unsigned-16 0)
                (ftype-&ref size_t () buf-len))]
           [len (ftype-ref size_t () buf-len 0)]
           [buf (code-unit-vector-alloc-ftype (ftype-ref size_t () buf-len 0))]
           ; now do the actual substitution
           [res2 (pcre2_substitute_16
                (regex-code regex)
                subject-addr
                subject-len
                start-offset
                (fxlogand (regex-flags regex) PCRE2_SUBSTITUTE_GLOBAL)
                match-data
                match-context
                replacement-addr
                replacement-len
                (code-unit-vector-&ref buf 0)
                (ftype-&ref size_t () buf-len))])
      (foreign-free (ftype-pointer-address buf-len))
      (make-pstring buf 0 (fx1- len))))

  (define pstring-make-regex
    (case-lambda
      [(bs) (pstring-make-regex bs '())]
      [(bs flags)
        (let* ([errorcode (foreign-alloc 4)]
               [erroroffset (foreign-alloc 4)]
               [options (flags->options flags)]
               [code (pcre2_compile_16
                       (pstring-&ref bs 0)
                       (pstring-length bs)
                       options
                       errorcode
                       erroroffset
                       0)])
          (foreign-free errorcode)
          (foreign-free erroroffset)
          (if (fx=? code 0)
            #f
            (begin
              (pcre2_jit_compile_16 code PCRE2_JIT_COMPLETE)
              (finalizer (make-regex code (pcre2_match_data_create_from_pattern_16 code 0) (pstring) options)
                         (lambda (r)
                           (pcre2_code_free (regex-code r))
                           (pcre2_match_data_free_16 (regex-match-data r)))))))]))

  (define (pstring-regex-match regex subject on-match nomatch)
    (let* ([match-data (regex-match-data regex)]
           [rc (pcre2_match_16
                 (regex-code regex)
                 (pstring-&ref subject 0)
                 (pstring-length subject)
                 0
                 0
                 match-data
                 0)])
      (if (fx<? rc 0)
        #f
        (let* ([ovector (pcre2_get_ovector_pointer_16 match-data)]
               [count (pcre2_get_ovector_count_16 match-data)]
               [out (srfi:214:make-flexvector count)])
          (let recur ([i 0])
            (if (fx<? i count)
              (let ([sub-start (foreign-ref 'size_t ovector (fx* (fx* i 2) (foreign-sizeof 'size_t)))])
                ; TODO how do we get this value (PCRE2_UNSET) in a portable way?
                (if (= sub-start 18446744073709551615)
                  (begin (srfi:214:flexvector-set! out i nomatch)
                         (recur (fx1+ i)))
                  (let* ([sub-end (foreign-ref 'size_t ovector (fx* (fx1+ (fx* i 2)) (foreign-sizeof 'size_t)))]
                         [sub-len (fx- sub-end sub-start)]
                         [match-bs (make-pstring
                                     (pstring-buffer subject)
                                     (fx+ (pstring-offset subject) sub-start)
                                     sub-len)])
                    (srfi:214:flexvector-set! out i (on-match match-bs))
                    (recur (fx1+ i)))))
              out))))))

  (define (pstring-regex-search regex subject)
    (let* ([match-data (pcre2_match_data_create_from_pattern_16 (regex-code regex) 0)]
           [rc (pcre2_match_16
                 (regex-code regex)
                 (pstring-&ref subject 0)
                 (pstring-length subject)
                 0
                 0
                 match-data
                 0)])
      (if (fx<? rc 0)
        #f
        (let* ([ovector (pcre2_get_ovector_pointer_16 match-data)]
               [match-index (foreign-ref 'size_t ovector 0)])
          match-index))))


  ;;
  ;; Regex flags
  ;; 

  (define (flags->options flags)
    (define (flag->bitmask flag)
      (cond
        [(eq? flag 'dotAll) PCRE2_DOTALL]
        [(eq? flag 'ignoreCase) PCRE2_CASELESS]
        [(eq? flag 'multiline) PCRE2_MULTILINE]
        [(eq? flag 'global) PCRE2_SUBSTITUTE_GLOBAL]
        [else #x0]))

    (fold-right
      (lambda (flag acc)
        (let ([m (flag->bitmask (car flag))])
          (if (cdr flag) (fxlogor m acc) acc)))
      DEFAULT_FLAGS
      flags))

  ;;
  ;; PCRE bindings
  ;;

  (define PCRE2_ALT_BSUX #x00000002)
  (define PCRE2_EXTRA_ALT_BSUX #x00000020)
  (define PCRE2_SUBSTITUTE_GLOBAL #x00000100)
  (define PCRE2_CASELESS #x00000008)
  (define PCRE2_MULTILINE #x00000400)
  (define PCRE2_DOTALL #x00000020)
  
  (define PCRE2_SUBSTITUTE_OVERFLOW_LENGTH #x00001000)

  (define PCRE2_JIT_COMPLETE #x00000001)

  (define DEFAULT_FLAGS (fxlogor PCRE2_ALT_BSUX
                                 PCRE2_EXTRA_ALT_BSUX))

  (define (regex-has-flag regex flag)
    (fx>? (fxlogand (regex-flags regex) flag) 0))

  (define pcre-init
    (begin
      (load-shared-object "libpcre2-16.so")))

  (define pcre2_compile_16
    (foreign-procedure "pcre2_compile_16" ((* unsigned-16) size_t unsigned-32 uptr uptr uptr)
                       uptr))

  (define pcre2_match_data_create_from_pattern_16
    (foreign-procedure "pcre2_match_data_create_from_pattern_16" (uptr uptr)
                       uptr))

  (define pcre2_match_16
    (foreign-procedure "pcre2_match_16" (uptr (* unsigned-16) size_t size_t unsigned-32 uptr uptr)
                       int))

  (define pcre2_substitute_16
    (foreign-procedure "pcre2_substitute_16" (uptr (* unsigned-16) size_t size_t unsigned-32 uptr uptr (* unsigned-16) size_t (* unsigned-16) (* size_t))
                       int))

  (define pcre2_get_ovector_pointer_16
    (foreign-procedure "pcre2_get_ovector_pointer_16" (uptr)
                       uptr))

  (define pcre2_get_ovector_count_16
    (foreign-procedure "pcre2_get_ovector_count_16" (uptr)
                       unsigned-32))

  (define pcre2_match_data_free_16
    (foreign-procedure "pcre2_match_data_free_16" (uptr)
                       void))
  
  (define pcre2_code_free
    (foreign-procedure "pcre2_code_free_16" (uptr)
                       void))


  ; int pcre2_jit_compile(pcre2_code *code, uint32_t options);
  (define pcre2_jit_compile_16
    (foreign-procedure "pcre2_jit_compile_16" (uptr unsigned-32)
                       void))



  )
