(library (purs runtime test pstring-test)
  (export main)
  (import (chezscheme)
          (prefix (purs runtime) rt:)
          (prefix (purs runtime srfi :214) srfi:214:)
          (purs runtime pstring))

  (define check-raises
    (lambda (thunk)
      (call/cc
        (lambda (k)
          (with-exception-handler
            (lambda (e) (k #t))
            (lambda () (begin (thunk) #f)))))))

  ; Creates a string slice by first making a bigger string and then slicing
  ; to get back a slice that equals the original input.
  (define (lit s)
    (pstring-slice
      (pstring-concat
        (string->pstring "123")
        (string->pstring s)
        (string->pstring "456"))
      3
      (fx+ 3 (pstring-length (string->pstring s)))))

  (define (replace s p r)
    (pstring-replace (lit s) (lit p) (lit r)))

  (define (replace-all s p r)
    (pstring-replace-all (lit s) (lit p) (lit r)))

  (define (split s p)
    (pstring-split (lit s) (lit p)))

  (define (regex-match r s)
    (pstring-regex-match (pstring-make-regex r) s (lambda (x) x) #f))

  (define (assert-all-pstring f)
    (assert (f (string->pstring "foobarbaz")))
    (assert (f (lit "foobarbaz")))
    (assert (f (pstring-concat (string->pstring "") (string->pstring "foobarbaz"))))
    (assert (f (pstring-concat (string->pstring "foobarbaz") (string->pstring ""))))
    (assert (f (pstring-concat (string->pstring "fooba") (string->pstring "rbaz"))))
    (assert (f (pstring-concat (lit "fooba") (lit "rbaz"))))
    (assert (f (pstring-concat (string->pstring "foo") (string->pstring "bar") (string->pstring "baz"))))
    (assert (f (pstring-concat (lit "foo") (lit "bar") (lit "baz")))))

  (define main
    (lambda ()

      ;; reffing
      (assert (check-raises (lambda () (pstring-ref-code-point (lit "") 0))))
      (assert (fx=? 228 (pstring-ref-code-point (lit "ä") 0)))

      ;; length in code units
      (assert (fx=? 0 (pstring-length (lit ""))))
      (assert (fx=? 1 (pstring-length (lit "a"))))
      (assert (fx=? 1 (pstring-length (lit "ä"))))
      (assert (fx=? 2 (pstring-length (lit "ää"))))
      (assert (fx=? 2 (pstring-length (lit "🍔"))))
      (assert (fx=? 4 (pstring-length (lit "🍔🍔"))))
      (assert-all-pstring (lambda (s) (fx=? (pstring-length s) 9)))

      ;; length in code points
      (assert (fx=? 0 (pstring-length-code-points (lit ""))))
      (assert (fx=? 1 (pstring-length-code-points (lit "a"))))
      (assert (fx=? 1 (pstring-length-code-points (lit "ä"))))
      (assert (fx=? 2 (pstring-length-code-points (lit "äが"))))
      (assert (fx=? 2 (pstring-length-code-points (lit "🍔🍔"))))
      (assert-all-pstring (lambda (s) (fx=? (pstring-length-code-points s) 9)))


      ;; slicing
      (assert (pstring=?
                (pstring-slice (lit "foo") 0)
                (string->pstring "foo")))
      (assert (pstring=?
                (pstring-slice (lit "foo") 1)
                (string->pstring "oo")))
      (assert (pstring=?
                (pstring-slice (lit "foo bar") 4 7)
                (string->pstring "bar")))
      (assert (pstring=?
                (pstring-slice (lit "foo bar") 4 100)
                (string->pstring "bar")))
      (assert (pstring=?
                (pstring-slice (lit "foo") -1)
                (string->pstring "foo")))
      (assert-all-pstring (lambda (s) (pstring=? (pstring-slice s 2 6) (string->pstring "obar"))))

      ; Entirely in the prefix
      (let ([s (pstring-concat (string->pstring "foo") (string->pstring "bar"))])
        (assert (pstring=? (pstring-take s 3) (string->pstring "foo")))
        (assert (not (pstring-compact? s))))

      ; Reachable within prefix and suffix
      (let ([s (pstring-concat (string->pstring "foo") (string->pstring "bar"))])
        (assert (pstring=? (pstring-take s 4) (string->pstring "foob")))
        (assert (pstring-compact? s)))

      ; Reachable within suffix
      (let ([s (pstring-concat (string->pstring "foo") (string->pstring "bar"))])
        (assert (pstring=? (pstring-drop s 3) (string->pstring "bar")))
        (assert (not (pstring-compact? s))))

      ;; take
      (assert (pstring=? (pstring-take (lit "foo") -1) (lit "")))
      (assert (pstring=? (pstring-take (lit "foo") 0) (lit "")))
      (assert (pstring=? (pstring-take (lit "foo") 1) (lit "f")))
      (assert (pstring=? (pstring-take (lit "foo") 2) (lit "fo")))
      (assert (pstring=? (pstring-take (lit "foo") -3) (lit "")))
      (assert (pstring=? (pstring-take (lit "foo") 10) (lit "foo")))

      ;; drop
      (assert (pstring=? (pstring-drop (lit "foo") -1) (lit "foo")))
      (assert (pstring=? (pstring-drop (lit "foo") 0) (lit "foo")))
      (assert (pstring=? (pstring-drop (lit "foo") 1) (lit "oo")))
      (assert (pstring=? (pstring-drop (lit "foo") 2) (lit "o")))
      (assert (pstring=? (pstring-drop (lit "foo") 10) (lit "")))
      (assert (pstring=? (pstring-take (pstring-drop (lit "foo bar baz") 4) 3) (lit "bar")))

      ;; index-of
      (assert (fx=? 0 (pstring-index-of (lit "foo bar") (lit ""))))
      (assert (fx=? 0 (pstring-index-of (lit "") (lit ""))))
      (assert (not (pstring-index-of (lit "foo bar") (lit "baz"))))
      (assert (fx=? 0 (pstring-index-of (lit "foo bar") (lit "f"))))
      (assert (fx=? 4 (pstring-index-of (lit "foo bar") (lit "b"))))
      (assert (fx=? 4 (pstring-index-of (lit "foo bar") (lit "bar"))))
      (assert (fx=? 3 (pstring-index-of (lit "fo foo") (lit "foo"))))
      (assert (fx=? 1 (pstring-index-of (lit "zza") (lit "za"))))
      (assert (fx=? 0 (pstring-index-of (lit "𝕒𝕓𝕔") (lit "𝕒"))))
      (assert (fx=? 2 (pstring-index-of (lit "𝕒𝕓𝕔") (lit "𝕓"))))
      (assert (fx=? 1 (pstring-index-of (lit "11112") (lit "1112"))))
      (assert-all-pstring (lambda (s) (fx=? 2 (pstring-index-of s (string->pstring "obar")))))

      ;; last-index-of
      (assert (fx=? 7 (pstring-last-index-of (lit "foo bar") (lit ""))))
      (assert (fx=? 0 (pstring-last-index-of (lit "") (lit ""))))
      (assert (not (pstring-index-of (lit "foo bar") (lit "baz"))))
      (assert (fx=? 8 (pstring-last-index-of (lit "foo bar foo") (lit "foo"))))
      (assert (fx=? 6 (pstring-last-index-of (lit "1112 1112") (lit "112"))))
      (assert-all-pstring (lambda (s) (fx=? 6 (pstring-last-index-of s (string->pstring "ba")))))

      ;; comparisons
      (assert (pstring=? (lit "abc") (lit "abc")))
      (assert (pstring=? (lit "𝕒𝕓𝕔") (lit "𝕒𝕓𝕔")))
      (assert (pstring<? (lit "fo") (lit "foo")))
      (assert (pstring<? (lit "a") (lit "z")))
      (assert (pstring<? (lit "az") (lit "z")))
      (assert (not (pstring<? (lit "foreign") (lit "else"))))
      (assert (not (pstring<? (lit "") (lit ""))))
      (assert (not (pstring>? (lit "a") (lit "a"))))
      (assert (not (pstring>? (lit "a") (lit "aa"))))
      (assert (not (pstring>? (lit "") (lit ""))))
      (assert (not (pstring>? (lit "else") (lit "foreign"))))
      (assert (pstring>=? (lit "") (lit "")))
      (assert (pstring>=? (lit "zoo") (lit "zoo")))
      (assert (pstring>=? (lit "zoo") (lit "zo")))
      (assert (not (pstring>=? (lit "zo") (lit "zoo"))))
      (assert (let ([s (lit "foo")]) (pstring>=? s s)))

      (assert (pstring<=? (lit "") (lit "")))
      (assert (not (pstring<=? (lit "zoo") (lit "zo"))))
      (assert (pstring<=? (lit "zo") (lit "zoo")))
      (assert (let ([s (lit "foo")]) (pstring<=? s s)))

      ;; casing
      (assert (pstring=? (pstring-downcase (lit "")) (lit "")))
      (assert (pstring=? (pstring-downcase (lit "foo")) (lit "foo")))
      (assert (pstring=? (pstring-downcase (lit "FOO")) (lit "foo")))
      (assert (pstring=? (pstring-upcase (lit "")) (lit "")))
      (assert (pstring=? (pstring-upcase (lit "FOO")) (lit "FOO")))
      (assert (pstring=? (pstring-upcase (lit "foo")) (lit "FOO")))
      (assert (not (pstring=? (pstring-upcase (lit "foo")) (lit "FOo"))))

      (assert (pstring=?
                (char-flexvector->pstring (srfi:214:flexvector))
                (lit "")))
      (assert (pstring=?
                (char-flexvector->pstring (srfi:214:flexvector #\f #\o #\o))
                (lit "foo")))

      (assert (srfi:214:flexvector=?
                equal?
                (pstring->char-flexvector (lit ""))
                (srfi:214:flexvector)))
      (assert (srfi:214:flexvector=?
                equal?
                (pstring->char-flexvector (lit "a"))
                (srfi:214:flexvector #\a)))
      (assert (srfi:214:flexvector=?
                equal?
                (pstring->char-flexvector (lit "foo"))
                (srfi:214:flexvector #\f #\o #\o)))
      (assert-all-pstring (lambda (s)
                            (srfi:214:flexvector=?
                              equal?
                              (pstring->char-flexvector s)
                              (srfi:214:flexvector #\f #\o #\o #\b #\a #\r #\b #\a #\z))))

      (assert (srfi:214:flexvector=?
                equal?
                (pstring->code-point-flexvector (lit ""))
                (srfi:214:flexvector)))
      (assert (srfi:214:flexvector=?
                equal?
                (pstring->code-point-flexvector (lit "b 𝐀𝐀"))
                (srfi:214:flexvector #x62 #x20 #x1D400 #x1D400)))

      (assert (pstring=? (replace "" "bar" "foo") (lit "")))
      (assert (pstring=? (replace "foo" "foo" "bar") (lit "bar")))
      (assert (pstring=? (replace "foo" "" "bar") (lit "foo")))
      (assert (pstring=? (replace "javascript" "java" "pure") (lit "purescript")))
      (assert (pstring=? (replace "foo bar" " bar" "") (lit "foo")))
      (assert (pstring=? (replace "𝕒𝕓𝕔" "𝕒" "a") (lit "a𝕓𝕔")))
      (assert (pstring=? (replace "abc" "b" "") (lit "ac")))
      (assert (pstring=? (replace "𝕒𝕓𝕔" "𝕓" "") (lit "𝕒𝕔")))
      (assert (pstring=? (replace "𝕒𝕓𝕔" "𝕔" "") (lit "𝕒𝕓")))
      (assert (pstring=? (replace "𝕒𝕓𝕔" "𝕒" "") (lit "𝕓𝕔")))

      (assert (pstring=? (replace-all "" "" "") (lit "")))
      (assert (pstring=? (replace-all "aaa" "a" "b") (lit "bbb")))
      (assert (pstring=? (replace-all "aaac" "a" "b") (lit "bbbc")))
      (assert (pstring=? (replace-all "foo foo foo" "foo" "bar") (lit "bar bar bar")))
      (assert (pstring=? (replace-all "abbc" "b" "") (lit "ac")))
      (assert (pstring=? (replace-all "abbbbbc" "b" "") (lit "ac")))
      (assert (pstring=? (replace-all "𝕒𝕓𝕓𝕔" "𝕓" "") (lit "𝕒𝕔")))
      (assert (pstring=? (replace-all "𝕒𝕓𝕓𝕔" "𝕓𝕓" "b") (lit "𝕒b𝕔")))
      (assert (pstring=? (pstring-replace-all
                              (pstring-drop (lit "foo 𝕒𝕓𝕓𝕔") 4)
                              (lit "𝕓𝕓")
                              (lit "b"))
                            (lit "𝕒b𝕔")))
      (assert-all-pstring (lambda (s)
                            (pstring=? (pstring-replace-all s (string->pstring "ba") (string->pstring "zz"))
                                       (string->pstring "foozzrzzz"))))

      (assert (srfi:214:flexvector=? pstring=? (split "" "a") (srfi:214:flexvector)))
      (assert (srfi:214:flexvector=? pstring=? (split "ab" "") (srfi:214:flexvector (lit "a") (lit "b"))))
      (assert (srfi:214:flexvector=? pstring=? (split "aabcc" "b") (srfi:214:flexvector (lit "aa") (lit "cc"))))

      (assert (pstring=? (pstring-trim (lit "   ")) (lit "")))
      (assert (pstring=? (pstring-trim (lit " a  ")) (lit "a")))
      (assert (pstring=? (pstring-trim (lit " ab ")) (lit "ab")))

      (assert (pstring=? (pstring-join-with (srfi:214:flexvector) (lit ",")) (lit "")))
      (assert (pstring=? (pstring-join-with (srfi:214:flexvector (lit "a")) (lit ",")) (lit "a")))
      (assert (pstring=? (pstring-join-with (srfi:214:flexvector (lit "a") (lit "b")) (lit ",")) (lit "a,b")))
      (assert (pstring=? (pstring-join-with (srfi:214:flexvector (lit "abc") (lit "def")) (lit " ")) (lit "abc def")))
      (assert (pstring=? (pstring-join-with (srfi:214:flexvector (lit "𝕒") (lit "𝕔")) (lit "𝕓")) (lit "𝕒𝕓𝕔")))
      (assert-all-pstring (lambda (s)
                            (pstring=? (pstring-join-with (srfi:214:flexvector s s) (lit ","))
                                       (string->pstring "foobarbaz,foobarbaz"))))

      (assert-all-pstring (lambda (s)
                            (srfi:214:flexvector=? pstring=?
                              (pstring-split s (lit "b"))
                              (srfi:214:flexvector (string->pstring "foo")
                                                   (string->pstring "ar")
                                                   (string->pstring "az")))))

      (assert-all-pstring (lambda (s)
                            (srfi:214:flexvector=? pstring=?
                              (pstring-split s (lit ""))
                              (srfi:214:flexvector (string->pstring "f")
                                                   (string->pstring "o")
                                                   (string->pstring "o")
                                                   (string->pstring "b")
                                                   (string->pstring "a")
                                                   (string->pstring "r")
                                                   (string->pstring "b")
                                                   (string->pstring "a")
                                                   (string->pstring "z")))))

      ; Code points

      (assert (fx=? 4 (pstring-length-code-points
                        (code-points->pstring #x61 #x16805 #x16A06 #x7A))))

      (assert (pstring=? (pstring-take-code-points (lit "foo") 0) (lit "")))
      (assert (pstring=? (pstring-take-code-points (lit "foo") -1) (lit "")))
      (assert (pstring=? (pstring-take-code-points (lit "") 2) (lit "")))
      (assert (pstring=? (pstring-take-code-points (lit "abc") 2) (lit "ab")))
      (assert (pstring=? (pstring-take-code-points (lit "abc") 3) (lit "abc")))
      (assert (pstring=? (pstring-take-code-points (lit "𝕒𝕓𝕔") 2) (lit "𝕒𝕓")))
      (assert (pstring=? (pstring-take-code-points (lit "𝕒𝕓𝕔") 3) (lit "𝕒𝕓𝕔")))
      (assert (pstring=? (pstring-take-code-points (lit "𝕒𝕓𝕔") 4) (lit "𝕒𝕓𝕔")))

      ;; regex
      (assert (srfi:214:flexvector=?
                pstring=?
                (regex-match (lit "foo") (lit "foo"))
                (srfi:214:flexvector (lit "foo"))))
      (assert (not (regex-match (lit "foo") (lit "bar"))))
      (assert (srfi:214:flexvector=?
                pstring=?
                (regex-match (lit "(a)(b)(c)") (lit "abc"))
                (srfi:214:flexvector (lit "abc") (lit "a") (lit "b") (lit "c"))))
      (assert (srfi:214:flexvector=?
                pstring=?
                (regex-match (lit "(a|b)(c)") (lit "ac"))
                (srfi:214:flexvector (lit "ac") (lit "a") (lit "c"))))
      (assert (srfi:214:flexvector=?
                pstring=?
                (regex-match (lit "(a|b)(c)") (lit "bc"))
                (srfi:214:flexvector (lit "bc") (lit "b") (lit "c"))))
      (assert (srfi:214:flexvector=?
                pstring=?
                (regex-match (string->pstring "^[a-z]+$") (lit "abc"))
                (srfi:214:flexvector (lit "abc"))))
      (assert (srfi:214:flexvector=?
                pstring=?
                (regex-match (lit "^[a-z]+$") (lit "abc"))
                (srfi:214:flexvector (lit "abc"))))
      (assert (not (srfi:214:flexvector-ref
                     (regex-match (lit "(a|(b))|(c)") (lit "ac"))
                     2)))

      (assert (pstring=?
                (pstring-regex-replace-by
                  (pstring-make-regex (lit "a") (list '(global . #f)))
                  (lit "aaa")
                  (lambda (m xs) (lit "b")))
                (lit "baa")))

      (assert (pstring=?
                (pstring-regex-replace-by
                  (pstring-make-regex (lit "foo") (list '(global . #f)))
                  (lit "bar")
                  (lambda (m xs) (lit "fail")))
                (lit "bar")))

      (assert (pstring=?
                (pstring-regex-replace-by
                  (pstring-make-regex (lit "[a-z]") (list '(global . #t)))
                  (string->pstring "")
                  (lambda (m xs) (lit "123")))
                (string->pstring "")))

      (assert (pstring=?
                (pstring-regex-replace-by
                  (pstring-make-regex (lit "aa") (list '(global . #t)))
                  (lit "bb")
                  (lambda (m xs) (lit "123")))
                (lit "bb")))

      (assert (pstring=?
                (pstring-regex-replace-by
                  (pstring-make-regex (lit "b") (list '(global . #t)))
                  (lit "abc")
                  (lambda (m xs) (lit "123")))
                (lit "a123c")))

      (assert (pstring=?
                (pstring-regex-replace-by
                  (pstring-make-regex (lit "a") (list '(global . #t)))
                  (lit "aaa")
                  (lambda (m xs) (lit "b")))
                (lit "bbb")))

      (assert (pstring=?
                (pstring-regex-replace-by
                  (pstring-make-regex (lit "([a-z]{2})") (list '(global . #t)))
                  (lit "aazz")
                  (lambda (m xs) (if (pstring=? m (lit "aa")) (lit "foo") (lit "bar"))))
                (lit "foobar")))

      (assert (pstring=?
                (pstring-regex-replace-by
                  (pstring-make-regex (lit "Hello, ([a-z]+)") (list '(global . #t)))
                  (lit "Hello, purescript")
                  (lambda (m xs) (srfi:214:flexvector-ref xs 0)))
                (lit "purescript")))

      ; replace
      (assert (pstring=?
                (pstring-regex-replace
                  (pstring-make-regex (lit "a") (list '(global . #f)))
                  (lit "aaa")
                  (lit "b"))
                (lit "baa")))

      (assert (pstring=?
                (pstring-regex-replace
                  (pstring-make-regex (lit "a") (list '(global . #t)))
                  (lit "aaa")
                  (lit "b"))
                (lit "bbb")))


      ; search
      (assert (not
                (pstring-regex-search
                  (pstring-make-regex (lit "a") (list '(global . #f)))
                  (lit "b"))))

      (assert (not
                (pstring-regex-search
                  (pstring-make-regex (lit "a") (list '(global . #f)))
                  (lit ""))))

      (assert (equal?
                (pstring-regex-search
                  (pstring-make-regex (lit "a") (list '(global . #f)))
                  (lit "abc"))
                0))

      (assert (equal?
                (pstring-regex-search
                  (pstring-make-regex (lit "b") (list '(global . #f)))
                  (lit "abc"))
                1))

      (assert (equal?
                (pstring-regex-search
                  (pstring-make-regex (lit "c") (list '(global . #f)))
                  (lit "abc"))
                2))

      ; regex-split

      (assert (srfi:214:flexvector=?
                pstring=?
                (pstring-regex-split (pstring-make-regex (lit "")) (lit ""))
                (srfi:214:flexvector)))

      (assert (srfi:214:flexvector=?
                pstring=?
                (pstring-regex-split (pstring-make-regex (lit "")) (lit "abc"))
                (srfi:214:flexvector (lit "a") (lit "b") (lit "c"))))

      (assert (srfi:214:flexvector=?
                pstring=?
                (pstring-regex-split (pstring-make-regex (lit ",")) (lit ""))
                (srfi:214:flexvector (lit ""))))

      (assert (srfi:214:flexvector=?
                pstring=?
                (pstring-regex-split (pstring-make-regex (lit ",")) (lit "a,b,"))
                (srfi:214:flexvector (lit "a") (lit "b") (lit ""))))

      (assert (srfi:214:flexvector=?
                pstring=?
                (pstring-regex-split (pstring-make-regex (lit ",")) (lit "a,b,c"))
                (srfi:214:flexvector (lit "a") (lit "b") (lit "c"))))

      (assert (srfi:214:flexvector=?
                pstring=?
                (pstring-regex-split (pstring-make-regex (lit "[,.]")) (lit "a,b,c.de"))
                (srfi:214:flexvector (lit "a") (lit "b") (lit "c") (lit "de"))))

      (assert (srfi:214:flexvector=?
                pstring=?
                (pstring-regex-split (pstring-make-regex (lit "🍔")) (lit "𝕒🍔𝕓🍔𝕔🍔de"))
                (srfi:214:flexvector (lit "𝕒") (lit "𝕓") (lit "𝕔") (lit "de"))))


      (assert (eqv? (eof-object) (pstring-cursor-peek-char (pstring->cursor (lit "")))))
      (assert (eqv? #\f (pstring-cursor-peek-char (pstring->cursor (lit "foo")))))

      (let ([cur (pstring->cursor (lit "foo"))])
        (assert (eqv? #\f (pstring-cursor-read-char cur)))
        (assert (eqv? #\o (pstring-cursor-read-char cur)))
        (assert (eqv? #\o (pstring-cursor-read-char cur)))
        (assert (eqv? (eof-object) (pstring-cursor-read-char cur))))

      ))
  )
