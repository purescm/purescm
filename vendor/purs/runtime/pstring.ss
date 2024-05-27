#!chezscheme
(library (purs runtime pstring)
  (export char-flexvector->pstring
          code-points->pstring
          number->pstring
          list->pstring
          pstring<=?
          pstring<?
          pstring=?
          pstring>=?
          pstring>?
          (rename (slice? pstring?))
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
          pstring-regex-split
          pstring-replace
          pstring-replace-all
          pstring-singleton
          pstring-slice
          pstring-split
          pstring->string
          pstring->symbol
          symbol->pstring
          pstring-take
          pstring-take-code-points
          pstring-trim
          pstring-uncons-char
          pstring-upcase
          regex-flags
          regex-source
          (rename (make-pstring-of-length make-slice))
          string->pstring
          pstring->cursor
          cursor->pstring
          pstring-cursor-read-char
          pstring-cursor-peek-char
          pstring-cursor-read-code-unit
          pstring-cursor-peek-code-unit
          pstring-cursor-read-code-point
          pstring-cursor-peek-code-point)
  (import (chezscheme)
          (prefix (purs runtime srfi :214) srfi:214:)
          (only (purs runtime finalizers) finalizer)
          (purs runtime pstring-buffer))

  (define-record pstring-base ())
  (define-record slice pstring-base
                 ((immutable buffer)
                  (immutable offset)
                  (immutable length)))
  (define-record concat pstring-base
                 ((mutable length)
                  (mutable rope)))

  (define (pstring-length str)
    (cond
      [(slice? str) (slice-length str)]
      [else (concat-length str)]))

  ;
  ; Constructors
  ; 

  (define empty-pstring (make-slice (make-immobile-bytevector 0) 0 0))

  ; Make a string of length `n` in code units
  (define (make-pstring-of-length n)
    (make-slice (pstring-buffer-alloc n) 0 n))

  ; Makes a string of one scheme char
  ;
  ; NOTE: this only takes in PS a char which are guaranteed to
  ; be only one code unit in size
  (define (pstring-singleton c)
    (let ([bv (pstring-buffer-alloc 1)])
      (pstring-buffer-set! bv 0 (char->integer c))
      (make-slice bv 0 1)))

  ; Makes a string from a list of chars
  ; NOTE: this only takes in PS chars which are guaranteed to
  ; be only one code unit in size
  (define (list->pstring . chars)
    (let* ([len (length chars)]
           [cv (pstring-buffer-alloc len)])
      (let loop ([i 0] [rest chars])
        (if (null? rest)
          cv
          (begin
            (pstring-buffer-set! cv i (char->integer (car rest)))
            (loop (fx1+ i) (cdr rest)))))
      (make-slice cv 0 len)))

  ; Macro that encodes literal scheme strings to `pstrings`
  ; at compile time.
  (define-syntax string->pstring
    (lambda (x)
      (syntax-case x ()
        [(string->pstring s)
         (let ([d (syntax->datum #'s)])
           (if (string? d)
             (let ([bv (string->utf16-immobile d)])
               #`(make-slice #,bv 0 #,(fx/ (bytevector-length bv) 2)))
             #'(let ([bv (string->utf16-immobile s)])
                 (make-slice bv 0 (fx/ (bytevector-length bv) 2)))))])))

  ; Makes a pstring from a list of code point scalar values.
  (define (code-points->pstring . xs)
    (let ([bv (string->utf16 (apply string (map integer->char xs)) (native-endianness))])
      (make-slice bv 0 (fx/ (bytevector-length bv) code-unit-length))))

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
      (make-slice bv 0 len)))

  ;
  ; Comparisons
  ;

  (define (pstring-empty? str)
    (fx=? (slice-length str) 0))

  ; Fast equality check based on object reference.
  (define (pstring-eq? x y)
    (and (fx=? (slice-length x) (slice-length y))
               (fx=? (slice-offset x) (slice-offset y))
               (eq? (slice-buffer x) (slice-buffer y))))

  (define (pstring=? x y)
    ; Assumes the buffers have the same length
    (define (pstring-equal-code-units? x y)
      (let ([cursor-x (pstring->cursor x)]
            [cursor-y (pstring->cursor y)])
        (let loop ([n 0]
                   [ch-x (pstring-cursor-read-code-unit cursor-x)]
                   [ch-y (pstring-cursor-read-code-unit cursor-y)])
          (or
            (or (eof-object? ch-x) (eof-object? ch-y))
            (and (fx=? ch-x ch-y)
                 (loop (fx1+ n)
                       (pstring-cursor-read-code-unit cursor-x)
                       (pstring-cursor-read-code-unit cursor-y)))))))
    (and
      (fx=? (slice-length x) (slice-length y))
      (or
        ; Do they point to the same object in memory?
        (and (fx=? (slice-offset x) (slice-offset y))
              (eq? (slice-buffer x) (slice-buffer y)))
        (pstring-equal-code-units? x y))))

  (define (pstring<? x y)
    (and
      (not (pstring-eq? x y))
      (let ([cursor-x (pstring->cursor x)] [cursor-y (pstring->cursor y)])
        (let loop ([ch-x (pstring-cursor-read-char cursor-x)]
                   [ch-y (pstring-cursor-read-char cursor-y)])
          (or
            (and (eof-object? ch-x)
                 (not (eof-object? ch-y)))
            (and (not (eof-object? ch-x))
                 (not (eof-object? ch-y))
                 (or (char<? ch-x ch-y)
                     (and (char=? ch-x ch-y)
                          (loop
                            (pstring-cursor-read-char cursor-x)
                            (pstring-cursor-read-char cursor-y))))))))))

  (define (pstring>? x y)
    (and
      (not (pstring-eq? x y))
      (let ([cursor-x (pstring->cursor x)]
            [cursor-y (pstring->cursor y)])
        (let loop ([ch-x (pstring-cursor-read-char cursor-x)]
                   [ch-y (pstring-cursor-read-char cursor-y)])
          (or
            ; is x longer than y?
            (and (not (eof-object? ch-x))
                 (eof-object? ch-y))
            (and
              (not (eof-object? ch-x))
              (not (eof-object? ch-y))
              (or (char>? ch-x ch-y)
                  (and (char=? ch-x ch-y)
                       (loop (pstring-cursor-read-char cursor-x)
                             (pstring-cursor-read-char cursor-y))))))))))

  (define (pstring<=? x y)
    (or
      (pstring-eq? x y)
      (let ([cursor-x (pstring->cursor x)]
            [cursor-y (pstring->cursor y)])
        (let loop ([ch-x (pstring-cursor-read-char cursor-x)]
                   [ch-y (pstring-cursor-read-char cursor-y)])
          (or
            (and
              (eof-object? ch-x)
              (eof-object? ch-y))
            (and (eof-object? ch-x)
                 (not (eof-object? ch-y)))
            (and
              (not (eof-object? ch-x))
              (not (eof-object? ch-y))
              (or (char<? ch-x ch-y)
                  (and (char=? ch-x ch-y)
                       (loop (pstring-cursor-read-char cursor-x)
                             (pstring-cursor-read-char cursor-y))))))))))

  (define (pstring>=? x y)
    (or
      (pstring-eq? x y)
      (let ([cursor-x (pstring->cursor x)]
            [cursor-y (pstring->cursor y)])
        (let loop ([ch-x (pstring-cursor-read-char cursor-x)]
                   [ch-y (pstring-cursor-read-char cursor-y)])
          (or
            (and
              (eof-object? ch-x)
              (eof-object? ch-y))
            (and (not (eof-object? ch-x))
                 (eof-object? ch-y))
            (and
              (not (eof-object? ch-x))
              (not (eof-object? ch-y))
              (or (char>? ch-x ch-y)
                  (and (char=? ch-x ch-y)
                       (loop (pstring-cursor-read-char cursor-x)
                             (pstring-cursor-read-char cursor-y))))))))))


  ;
  ; Accessors
  ; 

  ; Gets first code unit scalar value
  (define (pstring-ref-first str)
    (pstring-buffer-ref (slice-buffer str) (slice-offset str)))

  ; Get last code unit scalar value
  (define (pstring-ref-last str)
    (pstring-buffer-ref (slice-buffer str) (fx- (fx+ (slice-offset str) (slice-length str)) 1)))

  (define (pstring-ref-code-unit str n)
    (let ([bv (slice-buffer str)])
      (if (fx<? n (slice-length str))
        (pstring-buffer-ref bv (fx+ n (slice-offset str)))
        ; not enough bytes to read a full code unit
        (raise-continuable
          (make-message-condition
            (format "pstring-ref-code-unit ~d is not a valid index" n))))))

  ; Gets the char at index `n`.
  ;
  ; Constant-time ref, like string-ref.
  (define (pstring-ref str n)
    (integer->char (pstring-ref-code-unit str n)))

  ; Returns the address to the beginning of the slice
  (define (pstring-&ref str)
    (pstring-buffer-&ref (slice-buffer str) (slice-offset str)))

  ; Gets the code point scalar value at index `n`
  (define (pstring-ref-code-point str n)
    (let ([cur (pstring->cursor str)])
      (let loop ([cp (pstring-cursor-read-code-point cur)])
        (cond
          [(eof-object? cp)
           (raise-continuable
             (make-message-condition
               (format "pstring-ref-code-point: ~d is not a valid index" n)))]
          [(fx>? (pstring-cursor-offset cur) (fx+ (slice-offset str) n)) cp]
          [else (loop (pstring-cursor-read-code-point cur))]))))

  ; Length in code points
  (define (pstring-length-code-points s)
    (let ([cur (pstring->cursor s)])
      (let loop ([i 0] [cp (pstring-cursor-read-code-point cur)])
        (if (eof-object? cp)
          i
          (loop (fx1+ i) (pstring-cursor-read-code-point cur))))))

  ; Finds the starting index of first found `pattern`
  (define (pstring-index-of str pattern)
    (if (pstring-empty? pattern)
      0 ; special case for zero-length patterns
      (let loop ([candidate #f] ; the index of the first matching char
                 [str-idx 0]
                 [pattern-idx 0])
        (cond
          ; Found the end of the pattern, we are done
          [(fx=? (slice-length pattern) pattern-idx) candidate]
          ; In the middle of matching but we have no more input. No match found.
          [(fx=? (slice-length str) str-idx) #f]
          [else
            (let ([str-cu (pstring-ref-code-unit str str-idx)]
                  [pattern-cu (pstring-ref-code-unit pattern pattern-idx)])
              (if (fx=? str-cu pattern-cu)
                ; Found a match for char, remember this candidate and advance to next char
                (loop (or candidate str-idx) (fx1+ str-idx) (fx1+ pattern-idx))
                (loop #f
                      (if candidate
                        ; No match, rewind back to the possible next candidate
                        (fx1+ candidate)
                        ; No match and we didn't have any candidate so just advance to next char
                        (fx1+ str-idx))
                      0)))]))))

  ; Finds the index of last occurence of `pattern`.
  (define (pstring-last-index-of str pattern)
    (if (pstring-empty? pattern)
      (slice-length str)
      (let loop ([last-match-candidate #f]
                 [candidate #f]    ; the index of the first matching char
                 [str-idx 0]           ; haystack
                 [pattern-idx 0]) ; chars left to be found
        (cond
          [(and (fx<? str-idx (slice-length str)) (fx=? (slice-length pattern) pattern-idx))
           ; found a match but haystack not consumed, continue searching
           (loop candidate #f str-idx 0)]
          ; Reached the end of input and pattern, so we are done
          [(fx=? (slice-length pattern) pattern-idx) candidate]
          ; In the middle of matching but we have no more input.
          [(fx=? str-idx (slice-length str)) last-match-candidate]
          [else
            (let ([pc (pstring-ref-code-unit pattern pattern-idx)]
                  [ic (pstring-ref-code-unit str str-idx)])
              (if (fx=? pc ic)
                ; Found a match for char, advance to next char
                (loop last-match-candidate (or candidate str-idx) (fx1+ str-idx) (fx1+ pattern-idx))
                (loop last-match-candidate
                      #f
                      (if candidate
                        ; No match, rewind back to the possible next candidate
                        (fx1+ candidate)
                        ; No match and we didn't have any candidate so just advance to next char
                        (fx1+ str-idx))
                      0)))]))))


  ;
  ; Conversions
  ; 

  (define (pstring->string str)
    (utf16-immobile->string (slice-buffer str) (slice-offset str) (slice-length str)))

  (define pstring->number
    (case-lambda
      [(str) (string->number (pstring->string str))]
      [(str radix) (string->number (pstring->string str) radix)]))

  (define (pstring->symbol str)
    (string->symbol (pstring->string str)))

  (define (symbol->pstring sym)
    (string->pstring (symbol->string sym)))

  ; Turns a pstring to a flexvector of chars
  (define (pstring->char-flexvector str)
    (let* ([len (slice-length str)]
           [fv (srfi:214:make-flexvector len)])
      (let loop ([i 0] [rest str])
        (if (pstring-empty? rest)
          fv
          (let-values ([(c tail) (pstring-uncons-char rest)])
            (srfi:214:flexvector-set! fv i c)
            (loop (fx1+ i) tail))))))

  ; Turns a pstring to a list of chars
  (define (pstring->list str)
    (let ([cur (pstring->cursor str)])
      (let loop ([ch (pstring-cursor-read-char cur)] [ls '()])
        (if (eof-object? ch)
          (reverse ls)
          (loop (pstring-cursor-read-char cur) (cons ch ls))))))

  ; Turns a pstring to a flexvector of code points
  (define (pstring->code-point-flexvector str)
    (define (reverse-list->flexvector xs len)
      (let ([fv (srfi:214:make-flexvector len)])
        (let loop ([rest xs] [i (fx1- len)])
          (when (not (null? rest))
            (srfi:214:flexvector-set! fv i (car rest))
            (loop (cdr rest) (fx1- i))))
        fv))

    (let ([cur (pstring->cursor str)])
      (let loop ([ch (pstring-cursor-read-code-point cur)] [ls '()] [len 0])
        (if (eof-object? ch)
          (reverse-list->flexvector ls len)
          (loop (pstring-cursor-read-code-point cur) (cons ch ls) (fx1+ len))))))


  ;
  ; Joining & splitting
  ; 

  ; A linear-time string builder that builds a new pstring from a list
  ; of pstrings.
  (define (pstring-concat . xs)
    (let* ([len (fold-right (lambda (s a) (fx+ (slice-length s) a)) 0 xs)]
           [buf (pstring-buffer-alloc len)])
      (let loop ([i 0] [ls xs])
        (if (pair? ls)
          (let* ([str (car ls)]
                 [slen (slice-length str)])
            (pstring-buffer-copy! (slice-buffer str) (slice-offset str) buf i slen)
            (loop (fx+ i slen) (cdr ls)))
          (make-slice buf 0 len)))))

  ; Create a new pstring by joining a flexvector of pstrings using a separator
  (define (pstring-join-with xs separator)
    (let* ([len (srfi:214:flexvector-fold (lambda (len s) (fx+ len (slice-length s))) 0 xs)]
           [xs-count (srfi:214:flexvector-length xs)]
           [separator-count (if (fx=? xs-count 0) 0 (fx1- xs-count))]
           [separator-len (slice-length separator)]
           [bv-len (fx+ len (fx* separator-count separator-len))]
           [bv (pstring-buffer-alloc bv-len)])
      (let loop ([i 0]
                 [bi 0])
        (if (fx<? i xs-count)
          (let* ([s (srfi:214:flexvector-ref xs i)]
                 [len (slice-length s)])
            (if (fx>? i 0)
              (begin
                (pstring-buffer-copy!
                  (slice-buffer separator)
                  (slice-offset separator)
                  bv
                  (fx+ bi)
                  separator-len)
                (pstring-buffer-copy!
                  (slice-buffer s)
                  (slice-offset s)
                  bv
                  (fx+ bi separator-len)
                  len)
                (loop (fx1+ i) (fx+ bi len separator-len)))
              (begin
                (pstring-buffer-copy! (slice-buffer s) (slice-offset s) bv bi len)
                (loop (fx1+ i) (fx+ bi len)))))
          (make-slice bv 0 bv-len)))))

  ; Splits a pstring into a flexvector of pstrings using a pattern
  (define (pstring-split str pattern)
    (cond
      [(pstring-empty? str) (srfi:214:flexvector)]
      [(pstring-empty? pattern)
        (let* ([len (slice-length str)]
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
                (loop (cdr indices) (fx1+ i) (fx+ index (slice-length pattern)))))))]))


  ; 
  ; Slicing
  ;

  ; The primitive constant-time slice operation.
  ; Takes in a` start` index and optionally an `end` index.
  (define pstring-slice
    (case-lambda
      [(str start)
       (pstring-slice str start (slice-length str))]
      [(str start end)
        (let* ([start-index (fxmin (fxmax 0 start) (slice-length str))]
               [end-index (fxmin (fxmax 0 end) (slice-length str))]
               [len (fx- end-index start-index)])
          (if (fx<? len 0)
            empty-pstring
            (make-slice
              (slice-buffer str)
              (fx+ (slice-offset str) start-index)
              len)))]))

  (define (pstring-take str n)
    (pstring-slice str 0 n))

  (define (pstring-drop str n)
    (pstring-slice str n))

  ; Linear-time `take` on code points.
  (define (pstring-take-code-points str n)
    (if (fx<? n 1)
      empty-pstring
      (let ([cur (pstring->cursor str)])
        (let loop ([i n])
          (if (fx=? i 0)
            (pstring-take str (fx- (pstring-cursor-offset cur) (slice-offset str)))
            (let ([cp (pstring-cursor-read-code-point cur)])
              (cond
                [(eof-object? cp) str]
                [else (loop (fx1- i))])))))))

  ; Like pstring-drop but without bounds checks
  (define (pstring-unsafe-drop str n)
    (make-slice
      (slice-buffer str)
      (fx+ (slice-offset str) n)
      (fx- (slice-length str) n)))

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

    (let* ([cur (pstring->cursor str)]
           [suffix
             (let loop ([u (pstring-cursor-peek-code-unit cur)])
               (cond
                 [(eof-object? u) (cursor->pstring cur)]
                 [(whitespace? u) (begin
                                    (pstring-cursor-read-code-unit cur)
                                    (loop (pstring-cursor-peek-code-unit cur)))]
                 [else (cursor->pstring cur)]))])
      (let loop ([rest suffix])
        (if (pstring-empty? rest)
          rest
          (let ([last (pstring-ref-last rest)]
                [prefix (pstring-take rest (fx1- (slice-length rest)))])
            (if (whitespace? last) (loop prefix) rest))))))

  ; Replace the first occurence of `pattern` with `replacement`
  (define (pstring-replace str pattern replacement)
    (if (pstring-empty? pattern)
      str
      (let ([i (pstring-index-of str pattern)])
        (if (not i)
          str
          (let* ([len (fx+ (fx- (slice-length str) (slice-length pattern))
                           (slice-length replacement))]
                 [bv (pstring-buffer-alloc len)])
            (pstring-buffer-copy! (slice-buffer str) (slice-offset str) bv 0 i)
            (pstring-buffer-copy! (slice-buffer replacement)
                              (slice-offset replacement)
                              bv
                              i
                              (slice-length replacement))
            (pstring-buffer-copy! (slice-buffer str)
                              (fx+ (slice-offset str) i (slice-length pattern))
                              bv
                              (fx+ i (slice-length replacement))
                              (fx- (slice-length str) i (slice-length pattern)))
            (make-slice bv 0 len))))))

  ; Find all occurences and return their indices as a list
  (define (all-index-of str pattern)
    (let go ([start 0])
      (let ([slice (pstring-drop str start)])
        (if (or (pstring-empty? str) (pstring-empty? pattern))
          '()
          (let ([i (pstring-index-of slice pattern)])
            (if i
              (cons (fx+ start i)
                    (go (fx+ start i (slice-length pattern))))
              '()))))))

  ; Replace all occurences of `pattern` with `replacement`
  (define (pstring-replace-all str pattern replacement)
    (if (pstring-empty? pattern)
      str
      (let* ([is (all-index-of str pattern)]
             [replacements-delta (fx* (length is)
                                      (fx- (slice-length pattern)
                                           (slice-length replacement)))]
             [len (fx- (slice-length str) replacements-delta)]
             [bv (pstring-buffer-alloc len)])
        (let loop ([stri 0] ; where we are at str
                   [bvi 0] ; where we are at bv
                   [rest is])
          (if (null? rest)
            ; copy the left-overs into place
            (pstring-buffer-copy! (slice-buffer str)
                                    (fx+ (slice-offset str) stri)
                                    bv
                                    bvi
                                    (fx- (slice-length str) stri))
            (let* ([i (car rest)] [before-len (fx- i stri)])
              ; copy stuff before the match
              (pstring-buffer-copy! (slice-buffer str)
                                (fx+ (slice-offset str) stri)
                                bv
                                bvi
                                before-len)
              ; the replacement itself
              (pstring-buffer-copy! (slice-buffer replacement)
                                (slice-offset replacement)
                                bv
                                (fx+ bvi before-len)
                                (slice-length replacement))
              (loop
                (fx+ i (slice-length pattern))
                (fx+ (fx+ bvi before-len) (slice-length replacement))
                (cdr rest)))))
        (make-slice bv 0 len))))


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
                              (make-slice
                                (slice-buffer sub-str)
                                (fx+ (slice-offset match) (slice-length match))
                                (fx- (slice-length sub-str)
                                     (fx- (fx+ (slice-offset match) (slice-length match))
                                          (slice-offset sub-str))))
                              (fx+ delta (fx- (slice-length replacement) (slice-length match)))
                              (cons (cons match replacement) all-matches-reverse)))
                          (values delta (reverse all-matches-reverse)))))]
                  [(len) (fx+ (slice-length str) delta)]
                  [(bv) (pstring-buffer-alloc len)])
      (let loop ([stri (slice-offset str)] [bvi 0] [rest all-matches])
        (if (null? rest)
          ; copy the left-overs into place
          (pstring-buffer-copy!
            (slice-buffer str)
            stri
            bv
            bvi
            (fx- (slice-length str) (fx- stri (slice-offset str))))
          (let* ([match (caar rest)]
                 [replacement (cdar rest)]
                 [i (slice-offset match)]
                 [before-len (fx- i stri)])
            ; copy stuff before the match
            (pstring-buffer-copy! (slice-buffer str) stri bv bvi before-len)
            ; the replacement itself
            (pstring-buffer-copy!
              (slice-buffer replacement)
              (slice-offset replacement)
              bv
              (fx+ bvi before-len)
              (slice-length replacement))
            (loop
              (fx+ (slice-offset match) (slice-length match))
              (fx+ (fx+ bvi before-len) (slice-length replacement))
              (cdr rest)))))

          (make-slice bv 0 len)))

  ; Replace the first match using the replacement returned by calling `f` with the match.
  (define (pstring-regex-replace-single regex str f)
    (let ([matches (pstring-regex-match regex str identity #f)])
      (if (and matches (fx>? (srfi:214:flexvector-length matches) 0))
        (let* ([match (srfi:214:flexvector-ref matches 0)]
               [_ (srfi:214:flexvector-remove-front! matches)]
               [replacement (f match matches)]
               [delta (fx- (slice-length replacement) (slice-length match))]
               [len (fx+ (slice-length str) delta)]
               [buf (pstring-buffer-alloc len)])
          (let* ([before-len (fx- (slice-offset match) (slice-offset str))])
            ; copy stuff before the match
            (pstring-buffer-copy!
              (slice-buffer str)
              (slice-offset str)
              buf
              0
              before-len)
            ; the replacement itself
            (pstring-buffer-copy!
              (slice-buffer replacement)
              (slice-offset replacement)
              buf
              before-len
              (slice-length replacement))
            ; copy the stuff after the match
            (pstring-buffer-copy!
              (slice-buffer str)
              (fx+ (slice-offset match) (slice-length match))
              buf
              (fx+ before-len (slice-length replacement))
              (fx- (slice-length str)
                   (fx+ before-len (slice-length match))))
            (make-slice buf 0 len)))
        str)))

  ; Replace a match with `replacement`
  (define (pstring-regex-replace regex subject replacement)
    (let* ([match-data (pcre2_match_data_create_from_pattern_16 (regex-code regex) 0)]
           [buf-len (make-ftype-pointer size_t (foreign-alloc (foreign-sizeof 'size_t)))]
           [subject-addr (pstring-&ref subject)]
           [subject-len (slice-length subject)]
           [replacement-addr (pstring-&ref replacement)]
           [replacement-len (slice-length replacement)]
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
      (make-slice buf 0 (fx1- len))))

  (define (pstring-regex-split regex str)
    (cond
      [(pstring=? (regex-source regex) empty-pstring)
        (srfi:214:flexvector-map pstring-singleton (pstring->char-flexvector str))]
      [else
        (let* ([all-matches
                (let match-next ([sub-str str] [all-matches-reverse '()])
                  (let ([matches (pstring-regex-match regex sub-str identity #f)])
                    (if (and matches (fx>? (srfi:214:flexvector-length matches) 0))
                      (let* ([match (srfi:214:flexvector-ref matches 0)])
                        (match-next
                          ; Should slice be used here?
                          (make-slice
                            (slice-buffer sub-str)
                            (fx+ (slice-offset match) (slice-length match))
                            (fx- (slice-length sub-str)
                                 (fx- (fx+ (slice-offset match) (slice-length match))
                                      (slice-offset sub-str))))
                          (cons match all-matches-reverse)))
                      (reverse all-matches-reverse))))])
          (let* ([match-count (length all-matches)]
                 [fv (srfi:214:make-flexvector (fx1+ match-count))])
            (let loop ([stri (slice-offset str)] [rest all-matches] [match-idx 0])
              (if (null? rest)
                ; copy rest of the string as the last element
                (begin
                  (srfi:214:flexvector-set! fv match-idx (pstring-drop str (fx- stri (slice-offset str))))
                  fv)
                (let* ([prefix-len (fx- (slice-offset (car rest)) stri)]
                       [start-idx (fx- stri (slice-offset str))]
                       [end-idx (fx+ start-idx prefix-len)])
                  (srfi:214:flexvector-set! fv match-idx (pstring-slice str start-idx end-idx))
                  (loop (fx+ stri prefix-len (slice-length (car rest))) (cdr rest) (fx1+ match-idx)))))))]))

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
                       (slice-length str)
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
              (finalizer (make-regex code (pcre2_match_data_create_from_pattern_16 code 0) str options)
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
                 (slice-length subject)
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
                         [match-str (make-slice
                                     (slice-buffer subject)
                                     (fx+ (slice-offset subject) sub-start)
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
                 (slice-length subject)
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
    (case (machine-type)
      ; Not tested at all with Windows
      ; [(i3nt ti3nt a6nt ta6nt) (load-shared-object "libpcre2-16.dll")]
      [(i3osx ti3osx a6osx ta6osx arm64osx tarm64osx) (load-shared-object "libpcre2-16.dylib")]
      [(i3le ti3le a6le ta6le arm64le tarm64le) (load-shared-object "libpcre2-16.so")]
      [else (error "purescm"
                   (format "Failed to load libpcre2: machine-type ~s not supported." (machine-type)))]))

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

  ;
  ; Cursor
  ;
  ; An abstraction much like scheme ports that allows looping over a string
  ; without doing extra allocations
  ; 

  (define-record pstring-cursor (buffer offset end-offset))

  (define (pstring->cursor str)
    (make-pstring-cursor
      (slice-buffer str)
      (slice-offset str)
      (fx+ (slice-offset str) (slice-length str))))

  ; Slice the underlying buffer starting from where the cursor is pointing to
  (define (cursor->pstring cur)
    (make-slice (pstring-cursor-buffer cur)
                (pstring-cursor-offset cur)
                (fx- (pstring-cursor-end-offset cur) (pstring-cursor-offset cur))))

  (define (pstring-cursor-read-code-unit cur)
    (if (fx<? (pstring-cursor-offset cur) (pstring-cursor-end-offset cur))
      (let ([offset (pstring-cursor-offset cur)])
        (set-pstring-cursor-offset! cur (fx1+ offset))
        (pstring-buffer-ref (pstring-cursor-buffer cur) offset))
      (eof-object)))

  (define (pstring-cursor-peek-code-unit cur)
    (if (fx<? (pstring-cursor-offset cur) (pstring-cursor-end-offset cur))
      (pstring-buffer-ref (pstring-cursor-buffer cur) (pstring-cursor-offset cur))
      (eof-object)))

  (define (pstring-cursor-read-char cur)
    (let ([cp (pstring-cursor-read-code-point cur)])
      (if (eof-object? cp)
        (eof-object)
        (integer->char cp))))

  (define (pstring-cursor-peek-char cur)
    (let ([cp (pstring-cursor-peek-code-point cur)])
      (if (eof-object? cp)
        (eof-object)
        (integer->char cp))))

  (define (pstring-cursor-read-code-point cur)
    (if (fx<? (pstring-cursor-offset cur) (pstring-cursor-end-offset cur))
      (let ([buf (pstring-cursor-buffer cur)]
            [offset (pstring-cursor-offset cur)]
            [end-offset (pstring-cursor-end-offset cur)])
        (let ([w1 (pstring-buffer-ref buf offset)])
          (cond
            ; Two-word encoding? Check for high surrogate
            [(and (fx<= #xD800 w1 #xDBFF) (fx>=? (fx- end-offset offset) 2))
             (let ([w2 (pstring-buffer-ref buf (fx1+ offset))])
               ; low surrogate?
               (if (fx<= #xDC00 w2 #xDFFF)
                 (begin
                   (set-pstring-cursor-offset! cur (fx+ offset 2))
                   (fx+
                     (fxlogor
                       (fxsll (fx- w1 #xD800) 10)
                       (fx- w2 #xDC00))
                     #x10000))
                 ; low surrogate not found, just return the high surrogate
                 (begin
                   (set-pstring-cursor-offset! cur (fx1+ offset))
                   65533)))]
            ; misplaced continuation word?
            [(fx<= #xDC00 w1 #xDFFF)
             (begin
               (set-pstring-cursor-offset! cur (fx1+ offset))
               65533)]
            ; one-word encoding
            [else
             (begin
               (set-pstring-cursor-offset! cur (fx1+ offset))
               w1)])))
      (eof-object)))

  (define (pstring-cursor-peek-code-point cur)
    (if (fx<? (pstring-cursor-offset cur) (pstring-cursor-end-offset cur))
      (let ([buf (pstring-cursor-buffer cur)]
            [offset (pstring-cursor-offset cur)]
            [end-offset (pstring-cursor-end-offset cur)])
        (let ([w1 (pstring-buffer-ref buf offset)])
          (cond
            ; Two-word encoding? Check for high surrogate
            [(and (fx<= #xD800 w1 #xDBFF) (fx>=? (fx- end-offset offset) 2))
             (let ([w2 (pstring-buffer-ref buf (fx1+ offset))])
               ; low surrogate?
               (if (fx<= #xDC00 w2 #xDFFF)
                 (fx+
                   (fxlogor
                     (fxsll (fx- w1 #xD800) 10)
                     (fx- w2 #xDC00))
                   #x10000)
                 65533))]
            ; misplaced continuation word?
            [(fx<= #xDC00 w1 #xDFFF) 65533]
            ; one-word encoding
            [else w1])))
      (eof-object)))

  )

