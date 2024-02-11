#!chezscheme
(library (purs runtime pstring)
  (export char-flexvector->pstring
          code-points->pstring
          number->pstring
          pstring
          pstring<=?
          pstring<?
          pstring=?
          pstring>=?
          pstring>?
          pstring?
          pstring->char-flexvector
          pstring->code-point-flexvector
          pstring-concat
          pstring-downcase
          pstring-drop
          pstring-empty?
          pstring-index-of
          pstring-join-with
          pstring-last-index-of
          pstring-length
          pstring-length-code-points
          pstring->list
          pstring-make-regex
          pstring->number
          pstring-ref
          pstring-ref-code-point
          pstring-regex-match
          pstring-regex-replace
          pstring-regex-replace-by
          pstring-regex-search
          pstring-replace
          pstring-replace-all
          pstring-singleton
          pstring-slice
          pstring-split
          pstring->string
          pstring->symbol
          pstring-take
          pstring-take-code-points
          pstring-trim
          pstring-uncons-char
          pstring-uncons-code-point
          pstring-upcase
          regex-flags
          regex-source
          (rename (make-pstring-of-length make-pstring))
          string->pstring)
  (import (chezscheme)
          (prefix (purs runtime srfi :214) srfi:214:)
          (only (purs runtime finalizers) finalizer)
          (purs runtime pstring-buffer))

  ; pstring is a slice of an immobile bytevector
  (define-structure
    (pstring buffer offset length))

  ;
  ; Constructors
  ; 

  (define empty-pstring (make-pstring (make-immobile-bytevector 0) 0 0))

  ; Make a string of length `n` in code units
  (define (make-pstring-of-length n)
    (make-pstring (pstring-buffer-alloc n) 0 n))

  ; Makes a string of one scheme char
  ;
  ; NOTE: this only takes in PS a char which are guaranteed to
  ; be only one code unit in size
  (define (pstring-singleton c)
    (let ([bv (pstring-buffer-alloc 1)])
      (pstring-buffer-set! bv 0 (char->integer c))
      (make-pstring bv 0 1)))

  ; Makes a string from a list of chars
  ; NOTE: this only takes in PS chars which are guaranteed to
  ; be only one code unit in size
  (define (pstring . chars)
    (let* ([len (length chars)]
           [cv (pstring-buffer-alloc len)])
      (let loop ([i 0] [rest chars])
        (if (null? rest)
          cv
          (begin
            (pstring-buffer-set! cv i (char->integer (car rest)))
            (loop (fx1+ i) (cdr rest)))))
      (make-pstring cv 0 len)))

  ; Macro that encodes literal scheme strings to `pstrings`
  ; at compile time.
  (define-syntax string->pstring
    (lambda (x)
      (syntax-case x ()
        [(string->pstring s)
         (let ([d (syntax->datum #'s)])
           (if (string? d)
             (let ([bv (string->utf16-immobile d)])
               #`(make-pstring #,bv 0 #,(fx/ (bytevector-length bv) 2)))
             #'(let ([bv (string->utf16-immobile s)])
                 (make-pstring bv 0 (fx/ (bytevector-length bv) 2)))))])))

  ; Makes a pstring from a list of code point scalar values.
  (define (code-points->pstring . xs)
    (let ([bv (string->utf16 (apply string (map integer->char xs)) (native-endianness))])
      (make-pstring bv 0 (fx/ (bytevector-length bv) code-unit-length))))

  ; Makes a pstring from a number
  (define number->pstring
    (case-lambda
      [(n) (string->pstring (number->string n))]
      [(n radix) (string->pstring (number->string n radix))]))

  ; Makes a pstring from a flexvector of scheme chars
  (define (char-flexvector->pstring v)
    (let* ([len (srfi:214:flexvector-length v)]
           [bv (pstring-buffer-alloc len)])
      (srfi:214:flexvector-for-each/index
        (lambda (i c)
          (pstring-buffer-set! bv i (char->integer c))) 
        v)
      (make-pstring bv 0 len)))

  ;
  ; Comparisons
  ;

  (define (pstring-empty? str)
    (fx=? (pstring-length str) 0))

  ; Fast equality check based on object reference.
  (define (pstring-eq? x y)
    (and (fx=? (pstring-length x) (pstring-length y))
               (fx=? (pstring-offset x) (pstring-offset y))
               (eq? (pstring-buffer x) (pstring-buffer y))))

  (define (pstring=? x y)
    ; Assumes the buffers have the same length
    (define (pstring-equal-code-units? x y)
      (let loop ([n 0] [tailx x] [taily y])
        (or
          (or (pstring-empty? tailx) (pstring-empty? taily))
          (let-values ([(hx tx) (pstring-uncons-code-unit tailx)]
                       [(hy ty) (pstring-uncons-code-unit taily)])
            (and (fx=? hx hy) (loop (fx1+ n) tx ty))))))
    (and
      (fx=? (pstring-length x) (pstring-length y))
      (or
        ; Do they point to the same object in memory?
        (and (fx=? (pstring-offset x) (pstring-offset y))
              (eq? (pstring-buffer x) (pstring-buffer y)))
        (pstring-equal-code-units? x y))))

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


  ;
  ; Accessors
  ; 

  ; Gets first code unit scalar value
  (define (pstring-ref-first str)
    (pstring-buffer-ref (pstring-buffer str) (pstring-offset str)))

  ; Get last code unit scalar value
  (define (pstring-ref-last str)
    (pstring-buffer-ref (pstring-buffer str) (fx- (fx+ (pstring-offset str) (pstring-length str)) 1)))

  ; Gets the char at index `n`.
  ;
  ; Constant-time ref, like string-ref.
  (define (pstring-ref str n)
    (define (pstring-ref-code-unit str n)
      (let ([bv (pstring-buffer str)])
        (if (fx<? n (pstring-length str))
          (pstring-buffer-ref bv (fx+ n (pstring-offset str)))
          ; not enough bytes to read a full code unit
          (raise-continuable
            (make-message-condition
              (format "pstring-ref-code-unit ~d is not a valid index" n))))))
    (integer->char (pstring-ref-code-unit str n)))

  ; Returns the address to the beginning of the slice
  (define (pstring-&ref str)
    (pstring-buffer-&ref (pstring-buffer str) (pstring-offset str)))

  ; Gets the code point scalar value at index `n`
  (define (pstring-ref-code-point str n)
    (let loop ([i 0] [cur str])
      (if (pstring-empty? cur)
        (raise-continuable
          (make-message-condition
            (format "pstring-ref-code-point: ~d is not a valid index" n))))
      (let-values ([(head tail) (pstring-uncons-code-point cur)])
        (if (fx=? i n)
          head
          (loop (fx1+ i) tail)))))

  ; Length in code points
  (define (pstring-length-code-points s)
    (let loop ([i 0] [cur s])
      (if (pstring-empty? cur)
        i
        (let-values ([(_ tail) (pstring-uncons-code-point cur)])
          (loop (fx1+ i) tail)))))

  ; Finds the starting index of first found `pattern`
  (define (pstring-index-of str pattern)
    (if (pstring-empty? pattern)
      0
      (let loop ([i 0]
                 [candidate #f]    ; the index of the first matching char
                 [hs str]           ; haystack
                 [demand pattern]) ; chars left to be found
        (cond
          ; Nothing is demanded, so we are done
          [(pstring-empty? demand) candidate]
          ; In the middle of matching but we have no more input. No match found.
          [(pstring-empty? hs) #f]
          [else
            (let-values ([(pc demand-rest) (pstring-uncons-code-unit demand)]
                         [(ic hs-rest) (pstring-uncons-code-unit hs)])
              (if (fx=? pc ic)
                ; Found a match for char, advance to next char
                (loop (fx1+ i) (or candidate i) hs-rest demand-rest)
                (if candidate
                  ; No match, rewind demand and start over at the same spot
                  (loop i #f hs pattern)
                  (loop (fx1+ i) #f hs-rest pattern))))]))))

  ; Finds the index of last occurence of `pattern`.
  (define (pstring-last-index-of str pattern)
    (if (pstring-empty? pattern)
      (pstring-length str)
      (let loop ([i 0]
                 [last-match-candidate #f]
                 [candidate #f]    ; the index of the first matching char
                 [hs str]           ; haystack
                 [demand pattern]) ; chars left to be found
        (cond
          [(and (not (pstring-empty? hs)) (pstring-empty? demand))
           ; found a match but haystack not consumed, continue searching
           (loop i candidate #f hs pattern)]
          ; Nothing is demanded, so we are done
          [(pstring-empty? demand) candidate]
          ; In the middle of matching but we have no more input.
          [(and (pstring-empty? hs) (not (pstring-empty? demand))) last-match-candidate]
          [else
            (let-values ([(pc demand-rest) (pstring-uncons-code-unit demand)]
                         [(ic hs-rest) (pstring-uncons-code-unit hs)])
              (if (fx=? pc ic)
                ; Found a match for char, advance to next char
                (loop (fx1+ i) last-match-candidate (or candidate i) hs-rest demand-rest)
                (if candidate
                  ; No match, rewind demand and start over at the same spot
                  (loop i last-match-candidate #f hs pattern)
                  (loop (fx1+ i) last-match-candidate #f hs-rest pattern))))]))))


  ;
  ; Conversions
  ; 

  (define (pstring->string str)
    (utf16-immobile->string (pstring-buffer str) (pstring-offset str) (pstring-length str)))

  (define pstring->number
    (case-lambda
      [(str) (string->number (pstring->string str))]
      [(str radix) (string->number (pstring->string str) radix)]))

  (define (pstring->symbol str)
    (string->symbol (pstring->string str)))

  ; Turns a pstring to a flexvector of chars
  (define (pstring->char-flexvector str)
    (let* ([len (pstring-length str)]
           [fv (srfi:214:make-flexvector len)])
      (let loop ([i 0] [rest str])
        (if (pstring-empty? rest)
          fv
          (let-values ([(c tail) (pstring-uncons-char rest)])
            (srfi:214:flexvector-set! fv i c)
            (loop (fx1+ i) tail))))))

  ; Turns a pstring to a list of chars
  (define (pstring->list str)
    (let loop ([rest str] [ls '()])
      (if (pstring-empty? rest)
        (reverse ls)
        (let-values ([(head tail) (pstring-uncons-code-point rest)])
          (loop tail (cons (integer->char head) ls))))))

  ; Turns a pstring to a flexvector of code points
  (define (pstring->code-point-flexvector str)
    (define (reverse-list->flexvector xs len)
      (let ([fv (srfi:214:make-flexvector len)])
        (let loop ([rest xs] [i (fx1- len)])
          (when (not (null? rest))
            (srfi:214:flexvector-set! fv i (car rest))
            (loop (cdr rest) (fx1- i))))
        fv))

    (let loop ([rest str] [ls '()] [len 0])
      (if (pstring-empty? rest)
        (reverse-list->flexvector ls len)
        (let-values ([(head tail) (pstring-uncons-code-point rest)])
          (loop tail (cons head ls) (fx1+ len))))))


  ;
  ; Joining & splitting
  ; 

  ; A linear-time string builder that builds a new pstring from a list
  ; of pstrings.
  (define (pstring-concat . xs)
    (let* ([len (fold-right (lambda (s a) (fx+ (pstring-length s) a)) 0 xs)]
           [buf (pstring-buffer-alloc len)])
      (let loop ([i 0] [ls xs])
        (if (pair? ls)
          (let* ([str (car ls)]
                 [slen (pstring-length str)])
            (pstring-buffer-copy! (pstring-buffer str) (pstring-offset str) buf i slen)
            (loop (fx+ i slen) (cdr ls)))
          (make-pstring buf 0 len)))))

  ; Create a new pstring by joining a flexvector of pstrings using a separator
  (define (pstring-join-with xs separator)
    (let* ([len (srfi:214:flexvector-fold (lambda (len s) (fx+ len (pstring-length s))) 0 xs)]
           [xs-count (srfi:214:flexvector-length xs)]
           [separator-count (if (fx=? xs-count 0) 0 (fx1- xs-count))]
           [separator-len (pstring-length separator)]
           [bv-len (fx+ len (fx* separator-count separator-len))]
           [bv (pstring-buffer-alloc bv-len)])
      (let loop ([i 0]
                 [bi 0])
        (if (fx<? i xs-count)
          (let* ([s (srfi:214:flexvector-ref xs i)]
                 [len (pstring-length s)])
            (if (fx>? i 0)
              (begin
                (pstring-buffer-copy!
                  (pstring-buffer separator)
                  (pstring-offset separator)
                  bv
                  (fx+ bi)
                  separator-len)
                (pstring-buffer-copy!
                  (pstring-buffer s)
                  (pstring-offset s)
                  bv
                  (fx+ bi separator-len)
                  len)
                (loop (fx1+ i) (fx+ bi len separator-len)))
              (begin
                (pstring-buffer-copy! (pstring-buffer s) (pstring-offset s) bv bi len)
                (loop (fx1+ i) (fx+ bi len)))))
          (make-pstring bv 0 bv-len)))))

  ; Splits a pstring into a flexvector of pstrings using a pattern
  (define (pstring-split str pattern)
    (cond
      [(pstring-empty? str) (srfi:214:flexvector)]
      [(pstring-empty? pattern)
        (let* ([len (pstring-length str)]
               [fv (srfi:214:make-flexvector len)])
          (let loop ([i 0] [rest str])
            (if (pstring-empty? rest)
              fv
              (let-values ([(c tail) (pstring-uncons-char rest)])
                (srfi:214:flexvector-set! fv i (pstring-singleton c))
                (loop (fx1+ i) tail)))))]
      [else
        (let* ([all-indices (all-index-of str pattern)]
               [vec (srfi:214:make-flexvector (fx1+ (length all-indices)))])
          (let loop ([indices all-indices]
                     [i 0]
                     [pi 0])
            (if (null? indices)
              (begin
                (srfi:214:flexvector-set! vec i (pstring-slice str pi))
                vec)
              (let ([index (car indices)])
                (srfi:214:flexvector-set! vec i (pstring-slice str pi index))
                (loop (cdr indices) (fx1+ i) (fx+ index (pstring-length pattern)))))))]))


  ; 
  ; Slicing
  ;

  ; The primitive constant-time slice operation.
  ; Takes in a` start` index and optionally an `end` index.
  (define pstring-slice
    (case-lambda
      [(str start)
       (pstring-slice str start (pstring-length str))]
      [(str start end)
        (let* ([start-index (fxmin (fxmax 0 start) (pstring-length str))]
               [end-index (fxmin (fxmax 0 end) (pstring-length str))]
               [len (fx- end-index start-index)])
          (if (fx<? len 0)
            empty-pstring
            (make-pstring
              (pstring-buffer str)
              (fx+ (pstring-offset str) start-index)
              len)))]))

  (define (pstring-take str n)
    (pstring-slice str 0 n))

  (define (pstring-drop str n)
    (pstring-slice str n))

  ; Linear-time `take` on code points.
  (define (pstring-take-code-points str n)
    (if (fx<? n 1)
      empty-pstring
      (let loop ([i n] [tail str])
        (cond
          [(and (fx>? i 0) (fx=? (pstring-length tail) 0)) str]
          [(fx=? i 0) (pstring-take str (fx- (pstring-length str) (pstring-length tail)))]
          [else
            (let-values ([(_ t) (pstring-uncons-code-point tail)])
              (loop (fx1- i) t))]))))

  ; Like pstring-drop but without bounds checks
  (define (pstring-unsafe-drop str n)
    (make-pstring
      (pstring-buffer str)
      (fx+ (pstring-offset str) n)
      (fx- (pstring-length str) n)))

  ; Returns the first char and the rest of the pstring
  (define (pstring-uncons-char str)
    (let-values ([(head tail) (pstring-uncons-code-unit str)])
      (values (integer->char head) tail)))

  ; Returns the first code unit scalar and the rest of the pstring
  (define (pstring-uncons-code-unit str)
    (if (pstring-empty? str)
      (raise-continuable
        (make-message-condition
          (format "pstring-uncons-code-unit: cannot uncons an empty pstring ~a" str)))
      (let ([w1 (pstring-ref-first str)]
            [tail (pstring-unsafe-drop str 1)])
        (values w1 tail))))

  ; Returns the first code point scalar and the rest of the pstring
  (define (pstring-uncons-code-point str)
    (if (pstring-empty? str)
      (raise-continuable
        (make-message-condition "pstring-uncons-code-point: pstring is empty"))
      (let* ([buf (pstring-buffer str)]
             [offset (pstring-offset str)]
             [w1 (pstring-buffer-ref buf offset)])
        (cond
          ; Two-word encoding? Check for high surrogate
          [(and (fx<= #xD800 w1 #xDBFF) (fx>=? (pstring-length str) 2))
           (let ([w2 (pstring-buffer-ref buf (fx1+ offset))])
             ; low surrogate?
             (if (fx<= #xDC00 w2 #xDFFF)
               (values
                 (fx+
                   (fxlogor
                     (fxsll (fx- w1 #xD800) 10)
                     (fx- w2 #xDC00))
                   #x10000)
                 (pstring-unsafe-drop str 2))
               ; low surrogate not found, just return the high surrogate
               (values w1 (pstring-unsafe-drop str 1))))]
          ; misplaced continuation word?
          [(fx<= #xDC00 w1 #xDFFF)
           (values w1 (pstring-unsafe-drop str 1))]
          ; one-word encoding
          [else (values w1 (pstring-unsafe-drop str 1))]))))


  ; 
  ; Modifications
  ;

  ; Slow downcasing that does an extra allocation to a scheme string
  (define (pstring-downcase str)
    (string->pstring (string-downcase (pstring->string str))))

  ; Slow upcasing that does an extra allocation to a scheme string
  (define (pstring-upcase str)
    (string->pstring (string-upcase (pstring->string str))))

  ; Trim whitespace around the pstring
  (define (pstring-trim str)
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

    (let ([suffix
            (let loop ([rest str])
              (if (pstring-empty? rest)
                rest
                (let-values ([(head tail) (pstring-uncons-code-unit rest)])
                  (if (whitespace? head)
                    (loop tail)
                    rest))))])
      (let loop ([rest suffix])
        (if (pstring-empty? rest)
          rest
          (let ([last (pstring-ref-last rest)]
                [prefix (pstring-take rest (fx1- (pstring-length rest)))])
            (if (whitespace? last) (loop prefix) rest))))))

  ; Replace the first occurence of `pattern` with `replacement`
  (define (pstring-replace str pattern replacement)
    (if (pstring-empty? pattern)
      str
      (let ([i (pstring-index-of str pattern)])
        (if (not i)
          str
          (let* ([len (fx+ (fx- (pstring-length str) (pstring-length pattern))
                           (pstring-length replacement))]
                 [bv (pstring-buffer-alloc len)])
            (pstring-buffer-copy! (pstring-buffer str) (pstring-offset str) bv 0 i)
            (pstring-buffer-copy! (pstring-buffer replacement)
                              (pstring-offset replacement)
                              bv
                              i
                              (pstring-length replacement))
            (pstring-buffer-copy! (pstring-buffer str)
                              (fx+ (pstring-offset str) i (pstring-length pattern))
                              bv
                              (fx+ i (pstring-length replacement))
                              (fx- (pstring-length str) i (pstring-length pattern)))
            (make-pstring bv 0 len))))))

  ; Find all occurences and return their indices as a list
  (define (all-index-of str pattern)
    (let go ([start 0])
      (let ([slice (pstring-drop str start)])
        (if (or (pstring-empty? str) (pstring-empty? pattern))
          '()
          (let ([i (pstring-index-of slice pattern)])
            (if i
              (cons (fx+ start i)
                    (go (fx+ start i (pstring-length pattern))))
              '()))))))

  ; Replace all occurences of `pattern` with `replacement`
  (define (pstring-replace-all str pattern replacement)
    (if (pstring-empty? pattern)
      str
      (let* ([is (all-index-of str pattern)]
             [replacements-delta (fx* (length is)
                                      (fx- (pstring-length pattern)
                                           (pstring-length replacement)))]
             [len (fx- (pstring-length str) replacements-delta)]
             [bv (pstring-buffer-alloc len)])
        (let loop ([stri 0] ; where we are at str
                   [bvi 0] ; where we are at bv
                   [rest is])
          (if (null? rest)
            ; copy the left-overs into place
            (pstring-buffer-copy! (pstring-buffer str)
                                    (fx+ (pstring-offset str) stri)
                                    bv
                                    bvi
                                    (fx- (pstring-length str) stri))
            (let* ([i (car rest)] [before-len (fx- i stri)])
              ; copy stuff before the match
              (pstring-buffer-copy! (pstring-buffer str)
                                (fx+ (pstring-offset str) stri)
                                bv
                                bvi
                                before-len)
              ; the replacement itself
              (pstring-buffer-copy! (pstring-buffer replacement)
                                (pstring-offset replacement)
                                bv
                                (fx+ bvi before-len)
                                (pstring-length replacement))
              (loop
                (fx+ i (pstring-length pattern))
                (fx+ (fx+ bvi before-len) (pstring-length replacement))
                (cdr rest)))))
        (make-pstring bv 0 len))))


  ; 
  ; Regex
  ;

  (define-structure
    (regex code match-data source flags))

  ; Replace match(es) using the replacement returned by calling `f` with the match.
  (define (pstring-regex-replace-by regex subject f)
    (if (regex-has-flag regex PCRE2_SUBSTITUTE_GLOBAL)
      (pstring-regex-replace-all regex subject f)
      (pstring-regex-replace-single regex subject f)))

  (define identity (lambda (x) x))

  ; Replace all matches using the replacement returned by calling `f` with the match.
  (define (pstring-regex-replace-all regex str f)
    (let*-values ([(delta all-matches)
                    (let match-next ([sub-str str] [delta 0] [all-matches-reverse '()])
                      (let ([matches (pstring-regex-match regex sub-str identity #f)])
                        (if (and matches (fx>? (srfi:214:flexvector-length matches) 0))
                          (let* ([match (srfi:214:flexvector-ref matches 0)]
                                 [_ (srfi:214:flexvector-remove-front! matches)]
                                 [replacement (f match matches)])
                            (match-next
                              ; Should slice be used here?
                              (make-pstring
                                (pstring-buffer sub-str)
                                (fx+ (pstring-offset match) (pstring-length match))
                                (fx- (pstring-length sub-str)
                                     (fx- (fx+ (pstring-offset match) (pstring-length match))
                                          (pstring-offset sub-str))))
                              (fx+ delta (fx- (pstring-length replacement) (pstring-length match)))
                              (cons (cons match replacement) all-matches-reverse)))
                          (values delta (reverse all-matches-reverse)))))]
                  [(len) (fx+ (pstring-length str) delta)]
                  [(bv) (pstring-buffer-alloc len)])
      (let loop ([stri (pstring-offset str)] [bvi 0] [rest all-matches])
        (if (null? rest)
          ; copy the left-overs into place
          (pstring-buffer-copy!
            (pstring-buffer str)
            stri
            bv
            bvi
            (fx- (pstring-length str) (fx- stri (pstring-offset str))))
          (let* ([match (caar rest)]
                 [replacement (cdar rest)]
                 [i (pstring-offset match)]
                 [before-len (fx- i stri)])
            ; copy stuff before the match
            (pstring-buffer-copy! (pstring-buffer str) stri bv bvi before-len)
            ; the replacement itself
            (pstring-buffer-copy!
              (pstring-buffer replacement)
              (pstring-offset replacement)
              bv
              (fx+ bvi before-len)
              (pstring-length replacement))
            (loop
              (fx+ (pstring-offset match) (pstring-length match))
              (fx+ (fx+ bvi before-len) (pstring-length replacement))
              (cdr rest)))))

          (make-pstring bv 0 len)))

  ; Replace the first match using the replacement returned by calling `f` with the match.
  (define (pstring-regex-replace-single regex str f)
    (let ([matches (pstring-regex-match regex str identity #f)])
      (if (and matches (fx>? (srfi:214:flexvector-length matches) 0))
        (let* ([match (srfi:214:flexvector-ref matches 0)]
               [_ (srfi:214:flexvector-remove-front! matches)]
               [replacement (f match matches)]
               [delta (fx- (pstring-length replacement) (pstring-length match))]
               [len (fx+ (pstring-length str) delta)]
               [buf (pstring-buffer-alloc len)])
          (let* ([before-len (fx- (pstring-offset match) (pstring-offset str))])
            ; copy stuff before the match
            (pstring-buffer-copy!
              (pstring-buffer str)
              (pstring-offset str)
              buf
              0
              before-len)
            ; the replacement itself
            (pstring-buffer-copy!
              (pstring-buffer replacement)
              (pstring-offset replacement)
              buf
              before-len
              (pstring-length replacement))
            ; copy the stuff after the match
            (pstring-buffer-copy!
              (pstring-buffer str)
              (fx+ (pstring-offset match) (pstring-length match))
              buf
              (fx+ before-len (pstring-length replacement))
              (fx- (pstring-length str)
                   (fx+ before-len (pstring-length match))))
            (make-pstring buf 0 len)))
        str)))

  ; Replace a match with `replacement`
  (define (pstring-regex-replace regex subject replacement)
    (let* ([match-data (pcre2_match_data_create_from_pattern_16 (regex-code regex) 0)]
           [buf-len (make-ftype-pointer size_t (foreign-alloc (foreign-sizeof 'size_t)))]
           [subject-addr (pstring-&ref subject)]
           [subject-len (pstring-length subject)]
           [replacement-addr (pstring-&ref replacement)]
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
           ; NOTE we allocate space for the trailing NULL code unit,
           ; but this should be fine because `pstring` has it's own `length`
           ; so it won't ever read that NULL code unit.
           [buf (pstring-buffer-alloc (ftype-ref size_t () buf-len 0))]
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
                   (pstring-buffer-&ref buf 0)
                   (ftype-&ref size_t () buf-len))])
      (foreign-free (ftype-pointer-address buf-len))
      ; Subtract NULL code unit from length
      (make-pstring buf 0 (fx1- len))))

  ; Compiles a regex pattern to a regex.
  (define pstring-make-regex
    (case-lambda
      [(str) (pstring-make-regex str '())]
      [(str flags)
        (let* ([errorcode (foreign-alloc 4)]
               [erroroffset (foreign-alloc 4)]
               [options (flags->options flags)]
               [code (pcre2_compile_16
                       (pstring-&ref str)
                       (pstring-length str)
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
                           (pcre2_code_free_16 (regex-code r))
                           (pcre2_match_data_free_16 (regex-match-data r)))))))]))

  ; Performs a regex match and then calls `on-match` for a successful match
  ; and `nomatch` for a non-match.
  (define (pstring-regex-match regex subject on-match nomatch)
    (let* ([match-data (regex-match-data regex)]
           [rc (pcre2_match_16
                 (regex-code regex)
                 (pstring-&ref subject)
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
                         [match-str (make-pstring
                                     (pstring-buffer subject)
                                     (fx+ (pstring-offset subject) sub-start)
                                     sub-len)])
                    (srfi:214:flexvector-set! out i (on-match match-str))
                    (recur (fx1+ i)))))
              out))))))

  ; Finds the index of the first match using a regex
  (define (pstring-regex-search regex subject)
    (let* ([match-data (pcre2_match_data_create_from_pattern_16 (regex-code regex) 0)]
           [rc (pcre2_match_16
                 (regex-code regex)
                 (pstring-&ref subject)
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


  ;
  ; Regex flags
  ; 

  (define (flags->options flags)
    (define (flag->bitmask flag)
      (cond
        [(eq? flag 'dotAll) PCRE2_DOTALL]
        [(eq? flag 'ignoreCase) PCRE2_CASELESS]
        [(eq? flag 'multiline) PCRE2_MULTILINE]
        [(eq? flag 'global) PCRE2_SUBSTITUTE_GLOBAL]
        [else 0]))

    (fold-right
      (lambda (flag acc)
        (let ([m (flag->bitmask (car flag))])
          (if (cdr flag) (fxlogor m acc) acc)))
      DEFAULT_FLAGS
      flags))

  ;
  ; PCRE bindings
  ;

  ; The used PCRE flags.
  ; 
  ; See full list of flags here:
  ; <https://github.com/PCRE2Project/pcre2/blob/7b649dce27f1adf67c4d266f3a051941dcf38cb3/src/pcre2.h.in>
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
  
  (define pcre2_code_free_16
    (foreign-procedure "pcre2_code_free_16" (uptr)
                       void))

  (define pcre2_jit_compile_16
    (foreign-procedure "pcre2_jit_compile_16" (uptr unsigned-32)
                       void))


  )

