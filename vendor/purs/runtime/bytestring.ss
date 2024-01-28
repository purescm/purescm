#!chezscheme
(library (purs runtime bytestring)
  (export bytestring-ref
          bytestring-length
          bytestring-uncons-code-unit
          bytestring-uncons-code-point
          (rename (make-bytestring-of-length make-bytestring))
          bytestring
          bytestring-empty?
          (rename ($bytestring? bytestring?))
          bytestring=?
          ; bytestring-hash
          bytestring-slice
          bytestring-trim
          bytestring-take
          bytestring-drop
          bytestring-index-of
          bytestring-last-index-of
          bytestring-singleton
          bytestring-join-with
          bytestring->string
          bytestring->number
          number->bytestring
          bytestring->symbol
          string->bytestring
          bytestring-ci=?
          bytestring-concat
          char-flexvector->bytestring
          bytestring->char-flexvector

          bytestring-replace
          bytestring-replace-all
          bytestring-split

          ;; code points
          bytestring-ref-code-point
          bytestring-length-code-points
          bytestring->list
          code-points->bytestring
          bytestring<?
          bytestring>?
          bytestring<=?
          bytestring>=?
          bytestring-foldcase
          bytestring-downcase
          bytestring-upcase
          bytestring-take-code-points

          bytestring-make-regex
          regex-source
          regex-flags
          bytestring-regex-match
          bytestring-regex-search
          bytestring-regex-replace
          bytestring-regex-replace-by)
  (import (chezscheme)
          (only (purs runtime finalizers) finalizer)
          (purs runtime code-unit-vector)
          (prefix (purs runtime srfi :214) srfi:214:))

  (define-structure
    ($bytestring buffer length))

  (define (make-bytestring buf offset len)
    (make-$bytestring (cons buf offset) len))

  (define (bytestring-buffer bs)
    (car ($bytestring-buffer bs)))

  (define (bytestring-offset bs)
    (cdr ($bytestring-buffer bs)))

  (define bytestring-length $bytestring-length)

  (define empty-bytestring (make-bytestring empty-code-unit-vector 0 0))

  (define (bytestring-empty? bs)
    (fx=? (bytestring-length bs) 0))

  ;; Do x and y point to the same object in memory?
  (define (bytestring-eq? x y)
    (and (fx=? (bytestring-length x) (bytestring-length y))
               (fx=? (bytestring-offset x) (bytestring-offset y))
               (eq? (bytestring-buffer x) (bytestring-buffer y))))

  ;; this is our version of `make-string`
  (define (make-bytestring-of-length n)
    (make-bytestring (code-unit-vector-alloc n) 0 n))

  ;; NOTE: this only takes in PS chars which are guaranteed to
  ;; be only one word in size (one code unit)
  (define (bytestring-singleton c)
    (let ([bv (code-unit-vector-alloc 1)])
      (code-unit-vector-set! bv 0 (char->integer c))
      (make-bytestring bv 0 1)))

  (define (bytestring=? x y)
    ;; Assumes the buffers have the same length
    (define (bytestring-equal-code-units? x y)
      (let loop ([n 0] [tailx x] [taily y])
        (or
          (or (bytestring-empty? tailx) (bytestring-empty? taily))
          (let-values ([(hx tx) ($bytestring-uncons-code-unit tailx)]
                       [(hy ty) ($bytestring-uncons-code-unit taily)])
            (and (fx=? hx hy) (loop (fx1+ n) tx ty))))))

    (if (fx=? (bytestring-length x) (bytestring-length y))
      (cond
        ;; Do they point to the same object in memory?
        [(and (fx=? (bytestring-offset x) (bytestring-offset y))
              (eq? (bytestring-buffer x) (bytestring-buffer y))) #t]
        [(bytestring-equal-code-units? x y) #t]
        [else #f])
      #f))

  ; ;; Mostly taken from Chez Scheme `newhash.ss` and updated to work with `bytestring`
  ; (define (bytestring-hash bs)
  ;   (define (hcabs hc) (if (fx< hc 0) (fxnot hc) hc))
  ;   (define (update hc k)
  ;     (let ([hc2 (fx+/wraparound hc (fxsll/wraparound (fx+/wraparound hc k) 10))])
  ;       (fxlogxor hc2 (fxsrl hc2 6))))
  ;   (define (bvupdate hc bv i)
  ;     (update hc (bytevector-u8-ref bv i)))
  ;   (let ([n (bytestring-length bs)]
  ;         [offset (bytestring-offset bs)]
  ;         [bv (bytestring-buffer bs)])
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

  (define-syntax string->bytestring
    (lambda (x)
      (syntax-case x ()
        [(string->bytestring s)
         (let ([d (syntax->datum #'s)])
           (if (string? d)
             #`(let ([bv #,(string->utf16 d 'little)])
                 (make-bytestring (bytevector->code-unit-vector bv) 0 (fx/ (bytevector-length bv) 2)))
             #'(let ([bv (string->utf16 s 'little)])
                 (make-bytestring (bytevector->code-unit-vector bv) 0 (fx/ (bytevector-length bv) 2)))))])))

  ; (define (string->bytestring s)
  ;   (let ([buf (string->code-unit-vector s)])
  ;     (make-bytestring buf 0 (code-unit-vector-length buf))))

  (define (bytestring->string bs)
    (code-unit-vector->string (bytestring-buffer bs) (bytestring-offset bs) (bytestring-length bs)))

  (define (bytestring-read-code-unit bs)
    (code-unit-vector-ref (bytestring-buffer bs) (bytestring-offset bs)))

  (define (bytestring-read-word-end bs)
    (code-unit-vector-ref (bytestring-buffer bs) (fx- (fx+ (bytestring-offset bs) (bytestring-length bs)) 1)))

  (define (bytestring-forward-code-units bs n)
    (make-bytestring
      (bytestring-buffer bs)
      (fx+ (bytestring-offset bs) n)
      (fx- (bytestring-length bs) n)))

  (define (bytestring-drop-code-units bs n)
    (make-bytestring
      (bytestring-buffer bs)
      (bytestring-offset bs)
      (fx- (bytestring-length bs) n)))

  (define ($bytestring-uncons-code-unit bs)
    (if (bytestring-empty? bs)
      (raise-continuable
        (make-message-condition
          (format "$bytestring-uncons-code-unit: cannot uncons an empty bytestring ~a" bs)))
      (let ([w1 (bytestring-read-code-unit bs)]
            [tail (bytestring-forward-code-units bs 1)])
        (values w1 tail))))

  (define (bytestring-uncons-code-unit bs)
    (let-values ([(head tail) ($bytestring-uncons-code-unit bs)])
      (values (integer->char head) tail)))

  ;; Constant-time ref, like string-ref.
  ;; Returns a scheme `char`.
  (define (bytestring-ref bs n)
    (define (bytestring-ref-code-unit bs n)
      (let ([bv (bytestring-buffer bs)])
        (if (fx<? n (bytestring-length bs))
          (code-unit-vector-ref bv (fx+ n (bytestring-offset bs)))
          ;; not enough bytes to read a full code unit
          (raise-continuable
            (make-message-condition
              (format "bytestring-ref-code-unit ~d is not a valid index" n))))))

    (integer->char (bytestring-ref-code-unit bs n)))

  (define bytestring-slice
    (case-lambda
      [(bs start)
       (bytestring-slice bs start (bytestring-length bs))]
      [(bs start end)
        (let* ([start-index (fxmin (fxmax 0 start) (bytestring-length bs))]
               [end-index (fxmin (fxmax 0 end) (bytestring-length bs))]
               [len (fx- end-index start-index)])
          (if (fx<? len 0)
            empty-bytestring
            (make-bytestring
              (bytestring-buffer bs)
              (fx+ (bytestring-offset bs) start-index)
              len)))]))

  (define (bytestring-take bs n)
    (bytestring-slice bs 0 n))

  (define (bytestring-drop bs n)
    (bytestring-slice bs n))

  (define (bytestring-trim bs)
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
              (if (bytestring-empty? rest)
                rest
                (let-values ([(head tail) ($bytestring-uncons-code-unit rest)])
                  (if (whitespace? head)
                    (loop tail)
                    rest))))])
      (let loop ([rest preceding])
        (if (bytestring-empty? rest)
          rest
          (let ([last (bytestring-read-word-end rest)]
                [prefix (bytestring-drop-code-units rest 1)])
            (if (whitespace? last) (loop prefix) rest))))))

  (define (bytestring-index-of bs pattern)
    (if (bytestring-empty? pattern)
      0
      (let loop ([i 0]
                 [candidate #f]    ; the index of the first matching char
                 [hs bs]           ; haystack
                 [demand pattern]) ; chars left to be found
        (cond
          ;; Nothing is demanded, so we are done
          [(bytestring-empty? demand) candidate]
          ;; In the middle of matching but we have no more input. No match found.
          [(bytestring-empty? hs) #f]
          [else
            (let-values ([(pc demand-rest) ($bytestring-uncons-code-unit demand)]
                         [(ic hs-rest) ($bytestring-uncons-code-unit hs)])
              (if (fx=? pc ic)
                ;; Found a match for char, advance to next char
                (loop (fx1+ i) (or candidate i) hs-rest demand-rest)
                (if candidate
                  ;; No match, rewind demand and start over at the same spot
                  (loop i #f hs pattern)
                  (loop (fx1+ i) #f hs-rest pattern))))]))))

  (define (bytestring-last-index-of bs pattern)
    (if (bytestring-empty? pattern)
      (bytestring-length bs)
      (let loop ([i 0]
                 [last-match-candidate #f]
                 [candidate #f]    ; the index of the first matching char
                 [hs bs]           ; haystack
                 [demand pattern]) ; chars left to be found
        (cond
          [(and (not (bytestring-empty? hs)) (bytestring-empty? demand))
           ;; found a match but haystack not consumed, continue searching
           (loop i candidate #f hs pattern)]
          ;; Nothing is demanded, so we are done
          [(bytestring-empty? demand) candidate]
          ;; In the middle of matching but we have no more input.
          [(and (bytestring-empty? hs) (not (bytestring-empty? demand))) last-match-candidate]
          [else
            (let-values ([(pc demand-rest) ($bytestring-uncons-code-unit demand)]
                         [(ic hs-rest) ($bytestring-uncons-code-unit hs)])
              (if (fx=? pc ic)
                ;; Found a match for char, advance to next char
                (loop (fx1+ i) last-match-candidate (or candidate i) hs-rest demand-rest)
                (if candidate
                  ;; No match, rewind demand and start over at the same spot
                  (loop i last-match-candidate #f hs pattern)
                  (loop (fx1+ i) last-match-candidate #f hs-rest pattern))))]))))

  (define (bytestring-concat . xs)
    (let* ([len (fold-right (lambda (s a) (fx+ (bytestring-length s) a)) 0 xs)]
           [buf (code-unit-vector-alloc len)])
      (let loop ([i 0] [ls xs])
        (if (pair? ls)
          (let* ([bs (car ls)]
                 [slen (bytestring-length bs)])
            (code-unit-vector-copy! (bytestring-buffer bs) (bytestring-offset bs) buf i slen)
            (loop (fx+ i slen) (cdr ls)))
          (make-bytestring buf 0 len)))))

  (define (bytestring-join-with xs separator)
    (let* ([len (srfi:214:flexvector-fold (lambda (len s) (fx+ len (bytestring-length s))) 0 xs)]
           [xs-count (srfi:214:flexvector-length xs)]
           [separator-count (if (fx=? xs-count 0) 0 (fx1- xs-count))]
           [separator-len (bytestring-length separator)]
           [bv-len (fx+ len (fx* separator-count separator-len))]
           [bv (code-unit-vector-alloc bv-len)])
      (let loop ([i 0]
                 [bi 0])
        (if (fx<? i xs-count)
          (let* ([s (srfi:214:flexvector-ref xs i)]
                 [len (bytestring-length s)])
            (if (fx>? i 0)
              (begin
                (code-unit-vector-copy!
                  (bytestring-buffer separator)
                  (bytestring-offset separator)
                  bv
                  (fx+ bi)
                  separator-len)
                (code-unit-vector-copy!
                  (bytestring-buffer s)
                  (bytestring-offset s)
                  bv
                  (fx+ bi separator-len)
                  len)
                (loop (fx1+ i) (fx+ bi len separator-len)))
              (begin
                (code-unit-vector-copy! (bytestring-buffer s) (bytestring-offset s) bv bi len)
                (loop (fx1+ i) (fx+ bi len)))))
          (make-bytestring bv 0 bv-len)))))

  (define bytestring->number
    (case-lambda
      [(bs) (string->number (bytestring->string bs))]
      [(bs radix) (string->number (bytestring->string bs) radix)]))

  (define number->bytestring
    (case-lambda
      [(n) (string->bytestring (number->string n))]
      [(n radix) (string->bytestring (number->string n radix))]))

  (define (bytestring->symbol bs)
    (string->symbol (bytestring->string bs)))

  (define (bytestring . chars)
    (let* ([len (length chars)]
           [cv (code-unit-vector-alloc len)])
      (let loop ([i 0] [rest chars])
        (if (null? rest)
          cv
          (begin
            (code-unit-vector-set! cv i (char->integer (car rest)))
            (loop (fx1+ i) (cdr rest)))))
      (make-bytestring cv 0 len)))

  ;; TODO add a proper implementation
  (define (bytestring-ci=? x y)
    (string-ci=? (bytestring->string x) (bytestring->string y)))

  (define (char-flexvector->bytestring v)
    (let* ([len (srfi:214:flexvector-length v)]
           [bv (code-unit-vector-alloc len)])
      (srfi:214:flexvector-for-each/index
        (lambda (i c)
          (code-unit-vector-set! bv i (char->integer c))) 
        v)
      (make-bytestring bv 0 len)))

  (define (bytestring->char-flexvector bs)
    (let* ([len (bytestring-length bs)]
           [fv (srfi:214:make-flexvector len)])
      (let loop ([i 0] [rest bs])
        (if (bytestring-empty? rest)
          fv
          (let-values ([(c tail) (bytestring-uncons-code-unit rest)])
            (srfi:214:flexvector-set! fv i c)
            (loop (fx1+ i) tail))))))

  (define (bytestring-&ref bs i)
    (if (code-unit-vector-pointer? (bytestring-buffer bs))
      (code-unit-vector-&ref (bytestring-buffer bs) (fx+ (bytestring-offset bs) i))
      (begin
        (let ([buffer (code-unit-vector-copy-ftype
                        (bytestring-buffer bs)
                        (bytestring-offset bs)
                        (bytestring-length bs))])
          (set-$bytestring-buffer! bs (cons buffer 0))
          (code-unit-vector-&ref buffer i)))))


  ;; 
  ;; Modifications
  ;;

  (define (bytestring-replace bs pattern replacement)
    (if (bytestring-empty? pattern)
      bs
      (let ([i (bytestring-index-of bs pattern)])
        (if (not i)
          bs
          (let* ([len (fx+ (fx- (bytestring-length bs) (bytestring-length pattern))
                           (bytestring-length replacement))]
                 [bv (code-unit-vector-alloc len)])
            (code-unit-vector-copy! (bytestring-buffer bs) (bytestring-offset bs) bv 0 i)
            (code-unit-vector-copy! (bytestring-buffer replacement)
                              (bytestring-offset replacement)
                              bv
                              i
                              (bytestring-length replacement))
            (code-unit-vector-copy! (bytestring-buffer bs)
                              (fx+ (bytestring-offset bs) i (bytestring-length pattern))
                              bv
                              (fx+ i (bytestring-length replacement))
                              (fx- (bytestring-length bs) i (bytestring-length pattern)))
            (make-bytestring bv 0 len))))))

  ; Find all occurences and return their indices as a list
  (define (all-index-of bs pattern)
    (let go ([start 0])
      (let ([slice (bytestring-drop bs start)])
        (if (or (bytestring-empty? bs) (bytestring-empty? pattern))
          '()
          (let ([i (bytestring-index-of slice pattern)])
            (if i
              (cons (fx+ start i)
                    (go (fx+ start i (bytestring-length pattern))))
              '()))))))

  (define (bytestring-replace-all bs pattern replacement)
    (if (bytestring-empty? pattern)
      bs
      (let* ([is (all-index-of bs pattern)]
             [replacements-delta (fx* (length is)
                                      (fx- (bytestring-length pattern)
                                           (bytestring-length replacement)))]
             [len (fx- (bytestring-length bs) replacements-delta)]
             [bv (code-unit-vector-alloc len)])
        (let loop ([bsi 0] ; where we are at bs
                   [bvi 0] ; where we are at bv
                   [rest is])
          (if (null? rest)
            ; copy the left-overs into place
            (code-unit-vector-copy! (bytestring-buffer bs)
                                    (fx+ (bytestring-offset bs) bsi)
                                    bv
                                    bvi
                                    (fx- (bytestring-length bs) bsi))
            (let* ([i (car rest)] [before-len (fx- i bsi)])
              ;; copy stuff before the match
              (code-unit-vector-copy! (bytestring-buffer bs)
                                (fx+ (bytestring-offset bs) bsi)
                                bv
                                bvi
                                before-len)
              ;; the replacement itself
              (code-unit-vector-copy! (bytestring-buffer replacement)
                                (bytestring-offset replacement)
                                bv
                                (fx+ bvi before-len)
                                (bytestring-length replacement))
              (loop
                (fx+ i (bytestring-length pattern))
                (fx+ (fx+ bvi before-len) (bytestring-length replacement))
                (cdr rest)))))
        (make-bytestring bv 0 len))))

  (define (bytestring-split bs pattern)
    (cond
      [(bytestring-empty? bs) (srfi:214:flexvector)]
      [(bytestring-empty? pattern)
        (let* ([len (bytestring-length bs)]
               [fv (srfi:214:make-flexvector len)])
          (let loop ([i 0] [rest bs])
            (if (bytestring-empty? rest)
              fv
              (let-values ([(c tail) (bytestring-uncons-code-unit rest)])
                (srfi:214:flexvector-set! fv i (bytestring-singleton c))
                (loop (fx1+ i) tail)))))]
      [else
        (let* ([all-indices (all-index-of bs pattern)]
               [vec (srfi:214:make-flexvector (fx1+ (length all-indices)))])
          (let loop ([indices all-indices]
                     [i 0]
                     [pi 0])
            (if (null? indices)
              (begin
                (srfi:214:flexvector-set! vec i (bytestring-slice bs pi))
                vec)
              (let ([index (car indices)])
                (srfi:214:flexvector-set! vec i (bytestring-slice bs pi index))
                (loop (cdr indices) (fx1+ i) (fx+ index (bytestring-length pattern)))))))]))


  ;;
  ;; Code points
  ;;

  ;; Low-level unconsing
  (define (bytestring-uncons-code-point bs)
    (if (bytestring-empty? bs)
      (raise-continuable
        (make-message-condition "bytestring-uncons-code-point: bytestring is empty"))
      (let* ([buf (bytestring-buffer bs)]
             [offset (bytestring-offset bs)]
             [w1 (code-unit-vector-ref buf offset)])
        (cond
          ;; Two-word encoding? Check for high surrogate
          [(and (fx<= #xD800 w1 #xDBFF) (fx>=? (bytestring-length bs) 2))
           (let ([w2 (code-unit-vector-ref buf (fx1+ offset))])
             ;; low surrogate?
             (if (fx<= #xDC00 w2 #xDFFF)
               (values
                 (fx+
                   (fxlogor
                     (fxsll (fx- w1 #xD800) 10)
                     (fx- w2 #xDC00))
                   #x10000)
                 (bytestring-forward-code-units bs 2))
               ;; low surrogate not found, just return the high surrogate
               (values w1 (bytestring-forward-code-units bs 1))))]
          ;; misplaced continuation word?
          [(fx<= #xDC00 w1 #xDFFF)
           (values w1 (bytestring-forward-code-units bs 1))]
          ;; one-word encoding
          [else (values w1 (bytestring-forward-code-units bs 1))]))))

  (define (bytestring-ref-code-point bs n)
    (let loop ([i 0] [cur bs])
      (if (bytestring-empty? cur)
        (raise-continuable
          (make-message-condition
            (format "bytestring-ref-code-point: ~d is not a valid index" n))))
      (let-values ([(head tail) (bytestring-uncons-code-point cur)])
        (if (fx=? i n)
          head
          (loop (fx1+ i) tail)))))

  ;; Length in code points
  (define bytestring-length-code-points
    (lambda (s)
      (let loop ([i 0] [cur s])
        (if (bytestring-empty? cur)
          i
          (let-values ([(_ tail) (bytestring-uncons-code-point cur)])
            (loop (fx1+ i) tail))))))

  ;; linear-time, returns a list of `char`s
  (define (bytestring->list bs)
    (let loop ([rest bs] [ls '()])
      (if (bytestring-empty? rest)
        (reverse ls)
        (let-values ([(head tail) (bytestring-uncons-code-point rest)])
          (loop tail (cons (integer->char head) ls))))))

  ;; Turns raw code point scalar values into a UTF-16 encoded bytestring.
  ;; Let's you generate a bytestring with invalid unicode.
  (define (code-points->bytestring . xs)
    (let* ([bvs (map utf16-encode xs)]
           [len (fold-left (lambda (total bv) (fx+ total (code-unit-vector-length bv))) 0 bvs)]
           [res (code-unit-vector-alloc len)])
      (let loop ([resi 0] [rest bvs])
        (if (null? rest)
          res
          (begin
            (code-unit-vector-copy! (car rest) 0 res resi (code-unit-vector-length (car rest)))
            (loop (fx+ resi (code-unit-vector-length (car rest))) (cdr rest)))))
      (make-bytestring res 0 len)))

  (define (bytestring<? x y)
    (and
      (not (bytestring-eq? x y))
      (let loop ([tailx x] [taily y])
        (or
          (and (bytestring-empty? tailx)
               (not (bytestring-empty? taily)))
          (and
            (not (bytestring-empty? tailx))
            (not (bytestring-empty? taily))
            (let-values ([(hx tx) (bytestring-uncons-code-point tailx)]
                         [(hy ty) (bytestring-uncons-code-point taily)])
              (let ([c1 (integer->char hx)]
                    [c2 (integer->char hy)])
                (or (char<? c1 c2)
                    (and (char=? c1 c2) (loop tx ty))))))))))

  (define (bytestring>? x y)
    (and
      (not (bytestring-eq? x y))
      (let loop ([tailx x] [taily y])
        (or
          ; is x longer than y?
          (and (not (bytestring-empty? tailx))
               (bytestring-empty? taily))
          (and
            (not (bytestring-empty? tailx))
            (not (bytestring-empty? taily))
            (let-values ([(hx tx) (bytestring-uncons-code-point tailx)]
                         [(hy ty) (bytestring-uncons-code-point taily)])
              (let ([c1 (integer->char hx)]
                    [c2 (integer->char hy)])
                (or (char>? c1 c2)
                    (and (char=? c1 c2) (loop tx ty))))))))))

  (define (bytestring<=? x y)
    (or
      (bytestring-eq? x y)
      (let loop ([tailx x] [taily y])
        (or
          (and
            (bytestring-empty? tailx)
            (bytestring-empty? taily))
          (and (bytestring-empty? tailx)
               (not (bytestring-empty? taily)))
          (and
            (not (bytestring-empty? tailx))
            (not (bytestring-empty? taily))
            (let-values ([(hx tx) (bytestring-uncons-code-point tailx)]
                         [(hy ty) (bytestring-uncons-code-point taily)])
              (let ([c1 (integer->char hx)]
                    [c2 (integer->char hy)])
                (or (char<? c1 c2)
                    (and (char=? c1 c2) (loop tx ty))))))))))

  (define (bytestring>=? x y)
    (or
      (bytestring-eq? x y)
      (let loop ([tailx x] [taily y])
        (or
          (and
            (bytestring-empty? tailx)
            (bytestring-empty? taily))
          (and (not (bytestring-empty? tailx))
               (bytestring-empty? taily))
          (and
            (not (bytestring-empty? tailx))
            (not (bytestring-empty? taily))
            (let-values ([(hx tx) (bytestring-uncons-code-point tailx)]
                         [(hy ty) (bytestring-uncons-code-point taily)])
              (let ([c1 (integer->char hx)]
                    [c2 (integer->char hy)])
                (or (char>? c1 c2)
                    (and (char=? c1 c2) (loop tx ty))))))))))

  (define (bytestring-foldcase bs)
    (string->bytestring (string-foldcase (bytestring->string bs))))

  (define (bytestring-downcase bs)
    (string->bytestring (string-downcase (bytestring->string bs))))

  (define (bytestring-upcase bs)
    (string->bytestring (string-upcase (bytestring->string bs))))

  (define (bytestring-take-code-points bs n)
    (if (fx<? n 1)
      empty-bytestring
      (let loop ([i n] [tail bs])
        (cond
          [(and (fx>? i 0) (fx=? (bytestring-length tail) 0)) bs]
          [(fx=? i 0) (bytestring-take bs (fx- (bytestring-length bs) (bytestring-length tail)))]
          [else
            (let-values ([(_ t) (bytestring-uncons-code-point tail)])
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
    (regex code source flags))

  (define (bytestring-regex-replace-by regex subject f)
    (if (regex-has-flag regex PCRE2_SUBSTITUTE_GLOBAL)
      (bytestring-regex-replace-all regex subject f)
      (bytestring-regex-replace-single regex subject f)))

  (define (bytestring-regex-replace-all regex bs f)
    (let*-values ([(delta all-matches)
                    (let match-next ([sub-bs bs] [delta 0] [all-matches-reverse '()])
                      (let ([matches (bytestring-regex-match regex sub-bs)])
                        (if (and matches (fx>? (srfi:214:flexvector-length matches) 0))
                          (let* ([match (srfi:214:flexvector-ref matches 0)]
                                 [_ (srfi:214:flexvector-remove-front! matches)]
                                 [replacement (f match matches)])
                            (match-next
                              ; Should slice be used here?
                              (make-bytestring
                                (bytestring-buffer sub-bs)
                                (fx+ (bytestring-offset match) (bytestring-length match))
                                (fx- (bytestring-length sub-bs)
                                     (fx- (fx+ (bytestring-offset match) (bytestring-length match))
                                          (bytestring-offset sub-bs))))
                              (fx+ delta (fx- (bytestring-length replacement) (bytestring-length match)))
                              (cons (cons match replacement) all-matches-reverse)))
                          (values delta (reverse all-matches-reverse)))))]
                  [(len) (fx+ (bytestring-length bs) delta)]
                  [(bv) (code-unit-vector-alloc len)])
      (let loop ([bsi (bytestring-offset bs)] [bvi 0] [rest all-matches])
        (if (null? rest)
          ; copy the left-overs into place
          (code-unit-vector-copy! (bytestring-buffer bs)
                                bsi
                                bv
                                bvi
                                (fx- (bytestring-length bs) (fx- bsi (bytestring-offset bs))))
          (let* ([match (caar rest)]
                 [replacement (cdr (car rest))]
                 [i (bytestring-offset match)]
                 [before-len (fx- i bsi)])
            ;; copy stuff before the match
            (code-unit-vector-copy! (bytestring-buffer bs) bsi bv bvi before-len)
            ;; the replacement itself
            (code-unit-vector-copy! (bytestring-buffer replacement)
                              (bytestring-offset replacement)
                              bv
                              (fx+ bvi before-len)
                              (bytestring-length replacement))
            (loop
              (fx+ (bytestring-offset match) (bytestring-length match))
              (fx+ (fx+ bvi before-len) (bytestring-length replacement))
              (cdr rest)))))

          (make-bytestring bv 0 len)))

  (define (bytestring-regex-replace-single regex bs f)
    (let ([matches (bytestring-regex-match regex bs)])
      (if (and matches (fx>? (srfi:214:flexvector-length matches) 0))
        (let* ([match (srfi:214:flexvector-ref matches 0)]
               [_ (srfi:214:flexvector-remove-front! matches)]
               [replacement (f match matches)]
               [delta (fx- (bytestring-length replacement) (bytestring-length match))]
               [len (fx+ (bytestring-length bs) delta)]
               [buf (code-unit-vector-alloc len)])
          (let* ([before-len (fx- (bytestring-offset match) (bytestring-offset bs))])
            ;; copy stuff before the match
            (code-unit-vector-copy!
              (bytestring-buffer bs)
              (bytestring-offset bs)
              buf
              0
              before-len)
            ;; the replacement itself
            (code-unit-vector-copy!
              (bytestring-buffer replacement)
              (bytestring-offset replacement)
              buf
              before-len
              (bytestring-length replacement))
            ; copy the stuff after the match
            (code-unit-vector-copy!
              (bytestring-buffer bs)
              (fx+ (bytestring-offset match) (bytestring-length match))
              buf
              (fx+ before-len (bytestring-length replacement))
              (fx- (bytestring-length bs)
                   (fx+ before-len (bytestring-length match))))
            (make-bytestring buf 0 len)))
        bs)))

  (define (bytestring-regex-replace regex subject replacement)
    (let* ([match-data (pcre2_match_data_create_from_pattern_16 (regex-code regex) 0)]
           [buf-len (make-ftype-pointer size_t (foreign-alloc (foreign-sizeof 'size_t)))]
           [start-offset 0]
           [match-context 0]
           ; first calculate the size of the output buffer by passing in 0 as the buf size
           ; and using PCRE2_SUBSTITUTE_OVERFLOW_LENGTH
           [_ (ftype-set! size_t () buf-len 0 0)]
           [res (pcre2_substitute_16
                (regex-code regex)
                (bytestring-&ref subject 0)
                (bytestring-length subject)
                start-offset
                (fxlogor (fxlogand (regex-flags regex) PCRE2_SUBSTITUTE_GLOBAL)
                         PCRE2_SUBSTITUTE_OVERFLOW_LENGTH)
                match-data
                match-context
                (bytestring-&ref replacement 0)
                (bytestring-length replacement)
                ; basically a null pointer
                (make-ftype-pointer unsigned-16 0)
                (ftype-&ref size_t () buf-len))]
           [len (ftype-ref size_t () buf-len 0)]
           [buf (code-unit-vector-alloc-ftype (ftype-ref size_t () buf-len 0))]
           ; now do the actual substitution
           [res2 (pcre2_substitute_16
                (regex-code regex)
                (bytestring-&ref subject 0)
                (bytestring-length subject)
                start-offset
                (fxlogand (regex-flags regex) PCRE2_SUBSTITUTE_GLOBAL)
                match-data
                match-context
                (bytestring-&ref replacement 0)
                (bytestring-length replacement)
                (code-unit-vector-&ref buf 0)
                (ftype-&ref size_t () buf-len))])
      (foreign-free (ftype-pointer-address buf-len))
      (make-bytestring buf 0 (fx1- len))))

  (define bytestring-make-regex
    (case-lambda
      [(bs) (bytestring-make-regex bs (make-hashtable symbol-hash eq?))]
      [(bs flags)
        (let* ([errorcode (foreign-alloc 4)]
               [erroroffset (foreign-alloc 4)]
               [options (flags->options flags)]
               [code (pcre2_compile_16
                       (bytestring-&ref bs 0)
                       (bytestring-length bs)
                       options
                       errorcode
                       erroroffset
                       0)])
          (foreign-free errorcode)
          (foreign-free erroroffset)
          (if (fx=? code 0)
            #f
            (finalizer (make-regex code (bytestring) options)
                       (lambda (o) (pcre2_code_free (regex-code o))))))]))

  (define (bytestring-regex-match regex subject)
    (let* ([match-data (pcre2_match_data_create_from_pattern_16 (regex-code regex) 0)]
           [rc (pcre2_match_16
                 (regex-code regex)
                 (bytestring-&ref subject 0)
                 (bytestring-length subject)
                 0
                 0
                 match-data
                 0)])
      (if (fx<? rc 0)
        (begin (pcre2_match_data_free_16 match-data) #f)
        (let* ([ovector (pcre2_get_ovector_pointer_16 match-data)]
               [count (pcre2_get_ovector_count_16 match-data)]
               [out (srfi:214:make-flexvector count)])
          (let recur ([i 0])
            (if (fx<? i count)
              (let ([sub-start (foreign-ref 'size_t ovector (fx* (fx* i 2) (foreign-sizeof 'size_t)))])
                ; TODO how do we get this value (PCRE2_UNSET) in a portable way?
                (if (= sub-start 18446744073709551615)
                  (begin (srfi:214:flexvector-set! out i #f)
                         (recur (fx1+ i)))
                  (let* ([sub-end (foreign-ref 'size_t ovector (fx* (fx1+ (fx* i 2)) (foreign-sizeof 'size_t)))]
                         [sub-len (fx- sub-end sub-start)]
                         [match-bs (make-bytestring
                                     (bytestring-buffer subject)
                                     (fx+ (bytestring-offset subject) sub-start)
                                     sub-len)])
                    (srfi:214:flexvector-set! out i match-bs)
                    (recur (fx1+ i)))))
              (begin
                (pcre2_match_data_free_16 match-data)
                out)))))))

  (define (bytestring-regex-search regex subject)
    (let* ([match-data (pcre2_match_data_create_from_pattern_16 (regex-code regex) 0)]
           [rc (pcre2_match_16
                 (regex-code regex)
                 (bytestring-&ref subject 0)
                 (bytestring-length subject)
                 0
                 0
                 match-data
                 0)])
      (if (fx<? rc 0)
        (begin (pcre2_match_data_free_16 match-data) #f)
        (let* ([ovector (pcre2_get_ovector_pointer_16 match-data)]
               [match-index (foreign-ref 'size_t ovector 0)])
          (pcre2_match_data_free_16 match-data)
          match-index))))


  ;;
  ;; Regex flags
  ;; 

  (define (flags->options flags)
    (define (hashtable-fold proc init hashtable)
      (let-values ([(keys vals) (hashtable-entries hashtable)])
        (let ([size (vector-length keys)])
          (let loop ([i 0] [result init])
            (if (fx>=? i size)
                result
                (loop
                  (fx+ i 1)
                  (proc
                    (vector-ref keys i)
                    (vector-ref vals i)
                    result)))))))

    (define (flag->bitmask flag)
      (cond
        [(eq? flag 'dotAll) PCRE2_DOTALL]
        [(eq? flag 'ignoreCase) PCRE2_CASELESS]
        [(eq? flag 'multiline) PCRE2_MULTILINE]
        [(eq? flag 'global) PCRE2_SUBSTITUTE_GLOBAL]
        [else #x0]))

    (hashtable-fold
      (lambda (k v acc)
        (let ([m (flag->bitmask k)])
          (if v (fxlogor m acc) acc)))
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


  )

