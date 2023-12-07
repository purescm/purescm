#!chezscheme
(library (purs runtime bytestring)
  (export bytestring-ref
          bytestring-length-code-units
          bytestring-uncons-code-unit
          bytestring-uncons-code-point
          (rename (make-bytestring-of-length make-bytestring))
          bytestring
          bytestring-empty?
          bytestring?
          bytestring=?
          bytestring-hash
          bytestring-slice
          bytestring-trim
          bytestring-take
          bytestring-drop
          bytestring-index-of
          bytestring-last-index-of
          bytestring-singleton
          bytestring-append
          bytestring-join-with
          bytestring->string
          bytestring->number
          number->bytestring
          bytestring->symbol
          string->bytestring
          bytestring-ci=?
          bytestring-cat-reverse
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
          )
  (import (only (rnrs bytevectors) native-endianness)
          (chezscheme)
          (prefix (purs runtime srfi :214) srfi:214:))

  ;; Immutable UTF-16 encoded slice into a bytevector buffer.
  ;; Uses the system's native endianness. Does not include a BOM.
  (define-structure
    (bytestring
      ;; the UTF-16 encoded bytevector
      buffer
      ;; start offset byte of the slice
      offset
      ;; size of the slice in bytes
      length))

  (define code-unit-length 2)

  (define empty-bytestring (make-bytestring (bytevector) 0 0))

  (define (bytestring-empty? bs)
    (fx=? (bytestring-length bs) 0))

  ;; Do x and y point to the same object in memory?
  (define (bytestring-eq? x y)
    (and (fx=? (bytestring-length x) (bytestring-length y))
               (fx=? (bytestring-offset x) (bytestring-offset y))
               (eq? (bytestring-buffer x) (bytestring-buffer y))))

  ;; this is our version of `make-string`
  (define (make-bytestring-of-length n)
    (make-bytestring (make-bytevector n) 0 n))

  ;; length in words
  (define (bytestring-length-code-units bs)
    (fx/ (bytestring-length bs) 2))

  ;; NOTE: this only takes in PS chars which are guaranteed to
  ;; be only one word in size (one code unit)
  (define (bytestring-singleton c)
    (let ([bv (make-bytevector code-unit-length)])
      (bytevector-u16-native-set! bv 0 (char->integer c))
      (make-bytestring bv 0 code-unit-length)))

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

  ;; Mostly taken from Chez Scheme `newhash.ss` and updated to work with `bytestring`
  (define (bytestring-hash bs)
    (define (hcabs hc) (if (fx< hc 0) (fxnot hc) hc))
    (define (update hc k)
      (let ([hc2 (fx+/wraparound hc (fxsll/wraparound (fx+/wraparound hc k) 10))])
        (fxlogxor hc2 (fxsrl hc2 6))))
    (define (bvupdate hc bv i)
      (update hc (bytevector-u8-ref bv i)))
    (let ([n (bytestring-length bs)]
          [offset (bytestring-offset bs)]
          [bv (bytestring-buffer bs)])
      (if (fx<= n 16)
          (do ([i 0 (fx+ i 1)] [hc 440697712 (bvupdate hc bv (fx+ i offset))])
            ((fx= i n) (hcabs hc)))
          (do ([i 0 (fx+ i 1)]
               [hc 440697712 (bvupdate hc bv (fx+ i offset))])
              ((fx= i 5)
               (do ([i (fx- n 5) (fx+ i 1)]
                    [hc hc (bvupdate hc bv (fx+ i offset))])
                   ((fx= i n)
                    (let ([stride (fxsrl n 4)])
                      (do ([i 5 (fx+ i stride)]
                           [hc hc (bvupdate hc bv (fx+ i offset))])
                          ((fx>= i n) (hcabs hc)))))))))))

  (define (string->bytestring s)
    (let ([bv (string->utf16 s (native-endianness))])
      (make-bytestring bv 0 (bytevector-length bv))))

  (define (bytestring->string bs)
    (let ([buf (make-bytevector (bytestring-length bs))])
      (bytevector-copy! (bytestring-buffer bs) (bytestring-offset bs) buf 0 (bytestring-length bs))
      (utf16->string buf (native-endianness))))

  (define (bytestring-read-code-unit bs)
    (bytevector-u16-native-ref (bytestring-buffer bs) (bytestring-offset bs)))

  (define (bytestring-read-word-end bs)
    (bytevector-u16-native-ref (bytestring-buffer bs) (fx- (fx+ (bytestring-offset bs) (bytestring-length bs)) code-unit-length)))

  (define (bytestring-forward-code-units bs n)
    (let ([i (fx* n code-unit-length)])
      (make-bytestring
        (bytestring-buffer bs)
        (fx+ (bytestring-offset bs) i)
        (fx- (bytestring-length bs) i))))

  (define (bytestring-drop-code-units bs n)
    (let ([i (fx* n code-unit-length)])
      (make-bytestring
        (bytestring-buffer bs)
        (bytestring-offset bs)
        (fx- (bytestring-length bs) i))))

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
      (let ([bv (bytestring-buffer bs)]
            [i (fx* n code-unit-length)])
        (if (fx<? i (bytestring-length bs))
          (bytevector-u16-native-ref bv (fx+ i (bytestring-offset bs)))
          ;; not enough bytes to read a full code unit
          (raise-continuable
            (make-message-condition
              (format "bytestring-ref-code-unit ~d is not a valid index" n))))))

    (integer->char (bytestring-ref-code-unit bs n)))

  (define bytestring-slice
    (case-lambda
      [(bs start)
       (bytestring-slice bs start (bytestring-length-code-units bs))]
      [(bs start end)
        (let* ([start-index (fxmin (fxmax 0 start) (bytestring-length-code-units bs))]
               [end-index (fxmin (fxmax 0 end) (bytestring-length-code-units bs))]
               [len (fx- end-index start-index)])
          (if (fx<? len 0)
            empty-bytestring
            (make-bytestring
              (bytestring-buffer bs)
              (fx+ (bytestring-offset bs) (fx* start-index code-unit-length))
              (fx* len code-unit-length))))]))

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
      (bytestring-length-code-units bs)
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

  (define (bytestring-append x y)
    (let* ([len (fx+ (bytestring-length x) (bytestring-length y))]
           [buf (make-bytevector len)])
      (bytevector-copy! (bytestring-buffer x) (bytestring-offset x) buf 0 (bytestring-length x))
      (bytevector-copy! (bytestring-buffer y) (bytestring-offset y) buf (bytestring-length x) (bytestring-length y))
      (make-bytestring buf 0 len)))

  (define (bytestring-cat-reverse xs)
    (let* ([len (fold-right (lambda (s a) (fx+ (bytestring-length s) a)) 0 xs)]
           [buf (make-bytevector len)])
      (let loop ([i len] [ls xs])
        (if (pair? ls)
          (let* ([bs (car ls)]
                 [slen (bytestring-length bs)]
                 [index (fx- i slen)])
            (bytevector-copy! (bytestring-buffer bs) (bytestring-offset bs) buf index slen)
            (loop index (cdr ls)))
          (make-bytestring buf 0 len)))))

  (define (bytestring-join-with xs separator)
    (let* ([len (srfi:214:flexvector-fold (lambda (len s) (fx+ len (bytestring-length s))) 0 xs)]
           [xs-count (srfi:214:flexvector-length xs)]
           [separator-count (if (fx=? xs-count 0) 0 (fx1- xs-count))]
           [separator-len (bytestring-length separator)]
           [bv-len (fx+ len (fx* separator-count separator-len))]
           [bv (make-bytevector bv-len)])
      (let loop ([i 0]
                 [bi 0])
        (if (fx<? i xs-count)
          (let* ([s (srfi:214:flexvector-ref xs i)]
                 [len (bytestring-length s)])
            (if (fx>? i 0)
              (begin
                (bytevector-copy!
                  (bytestring-buffer separator)
                  (bytestring-offset separator)
                  bv
                  (fx+ bi)
                  separator-len)
                (bytevector-copy!
                  (bytestring-buffer s)
                  (bytestring-offset s)
                  bv
                  (fx+ bi separator-len)
                  len)
                (loop (fx1+ i) (fx+ bi len separator-len)))
              (begin
                (bytevector-copy! (bytestring-buffer s) (bytestring-offset s) bv bi len)
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
    (string->bytestring (apply string chars)))

  ;; TODO add a proper implementation
  (define (bytestring-ci=? x y)
    (string-ci=? (bytestring->string x) (bytestring->string y)))

  (define (char-flexvector->bytestring v)
    (let* ([len (fx* code-unit-length (srfi:214:flexvector-length v))]
           [bv (make-bytevector len)])
      (srfi:214:flexvector-for-each/index
        (lambda (i c)
          (bytevector-u16-native-set! bv (fx* 2 i) (char->integer c))) 
        v)
      (make-bytestring bv 0 len)))

  (define (bytestring->char-flexvector bs)
    (let* ([len (bytestring-length-code-units bs)]
           [fv (srfi:214:make-flexvector len)])
      (let loop ([i 0] [rest bs])
        (if (bytestring-empty? rest)
          fv
          (let-values ([(c tail) (bytestring-uncons-code-unit rest)])
            (srfi:214:flexvector-set! fv i c)
            (loop (fx1+ i) tail))))))

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
                 [bv (make-bytevector len)]
                 [bi (fx* i code-unit-length)])
            (bytevector-copy! (bytestring-buffer bs) (bytestring-offset bs) bv 0 bi)
            (bytevector-copy! (bytestring-buffer replacement)
                              (bytestring-offset replacement)
                              bv
                              bi
                              (bytestring-length replacement))
            (bytevector-copy! (bytestring-buffer bs)
                              (fx+ (bytestring-offset bs) bi (bytestring-length pattern))
                              bv
                              (fx+ bi (bytestring-length replacement))
                              (fx- (bytestring-length bs) bi (bytestring-length pattern)))
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
                    (go (fx+ start i (bytestring-length-code-units pattern))))
              '()))))))

  (define (bytestring-replace-all bs pattern replacement)
    (if (bytestring-empty? pattern)
      bs
      (let* ([is (all-index-of bs pattern)]
             [replacements-delta (fx* (length is)
                                      (fx- (bytestring-length pattern)
                                           (bytestring-length replacement)))]
             [len (fx- (bytestring-length bs) replacements-delta)]
             [bv (make-bytevector len)])
        (let loop ([bsi 0] ; where we are at bs
                   [bvi 0] ; where we are at bv
                   [rest is])
          (if (null? rest)
            ; copy the left-overs into place
            (bytevector-copy! (bytestring-buffer bs)
                              (fx+ (bytestring-offset bs) bsi)
                              bv
                              bvi
                              (fx- (bytestring-length bs) bsi))
            (let* ([i (car rest)] [bi (fx* i code-unit-length)] [before-len (fx- bi bsi)])
              ;; copy stuff before the match
              (bytevector-copy! (bytestring-buffer bs)
                                (fx+ (bytestring-offset bs) bsi)
                                bv
                                bvi
                                before-len)
              ;; the replacement itself
              (bytevector-copy! (bytestring-buffer replacement)
                                (bytestring-offset replacement)
                                bv
                                (fx+ bvi before-len)
                                (bytestring-length replacement))
              (loop
                (fx+ bi (bytestring-length pattern))
                (fx+ (fx+ bvi before-len) (bytestring-length replacement))
                (cdr rest)))))
        (make-bytestring bv 0 len))))

  (define (bytestring-split bs pattern)
    (cond
      [(bytestring-empty? bs) (srfi:214:flexvector)]
      [(bytestring-empty? pattern)
        (let* ([len (bytestring-length-code-units bs)]
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
                (loop (cdr indices) (fx1+ i) (fx+ index (bytestring-length-code-units pattern)))))))]))


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
             [w1 (bytevector-u16-native-ref buf offset)])
        (cond
          ;; Two-word encoding? Check for high surrogate
          [(and (fx<= #xD800 w1 #xDBFF) (fx>=? (bytestring-length bs) 4))
           (let ([w2 (bytevector-u16-native-ref buf (fx+ offset 2))])
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
           [len (fold-left (lambda (total bv) (fx+ total (bytevector-length bv))) 0 bvs)]
           [res (make-bytevector len)])
      (let loop ([resi 0] [rest bvs])
        (if (null? rest)
          res
          (begin
            (bytevector-copy! (car rest) 0 res resi (bytevector-length (car rest)))
            (loop (fx+ resi (bytevector-length (car rest))) (cdr rest)))))
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
          [(fx=? i 0) (bytestring-take bs (fx/ (fx- (bytestring-length bs) (bytestring-length tail)) code-unit-length))]
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
      (let* ([bv (make-bytevector 4)]
             [code (fx- point #x10000)]
             [w1 (fxlogor #xD800 (fxsrl code 10))]
             [w2 (fxlogor #xDC00 (fxlogand code #x3FF))])
        (bytevector-u16-native-set! bv 0 w1)
        (bytevector-u16-native-set! bv 2 w2)
        bv)
      (let ([bv (make-bytevector 2)])
        (bytevector-u16-native-set! bv 0 point)
        bv)))

  )

