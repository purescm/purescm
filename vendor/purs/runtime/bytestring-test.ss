(library (purs runtime bytestring-test)
  (export main)
  (import (chezscheme)
          (prefix (purs runtime srfi :214) srfi:214:)
          (purs runtime bytestring))

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
    (bytestring-slice
      (bytestring-append
        (string->bytestring "123")
        (bytestring-append (string->bytestring s) (string->bytestring "456")))
      3
      (fx+ 3 (bytestring-length (string->bytestring s)))))

  (define (replace s p r)
    (bytestring-replace (lit s) (lit p) (lit r)))

  (define (replace-all s p r)
    (bytestring-replace-all (lit s) (lit p) (lit r)))

  (define (split s p)
    (srfi:214:flexvector-map bytestring->string (bytestring-split (lit s) (lit p))))


  (define main
    (lambda ()

      ;; reffing
      (assert (check-raises (lambda () (bytestring-ref-code-point (lit "") 0))))
      (assert (fx=? 228 (bytestring-ref-code-point (lit "ä") 0)))

      ;; length in code units
      (assert (fx=? 0 (bytestring-length (lit ""))))
      (assert (fx=? 1 (bytestring-length (lit "a"))))
      (assert (fx=? 1 (bytestring-length (lit "ä"))))
      (assert (fx=? 2 (bytestring-length (lit "ää"))))
      (assert (fx=? 2 (bytestring-length (lit "🍔"))))
      (assert (fx=? 4 (bytestring-length (lit "🍔🍔"))))

      ;; length in code points
      (assert (fx=? 0 (bytestring-length-code-points (lit ""))))
      (assert (fx=? 1 (bytestring-length-code-points (lit "a"))))
      (assert (fx=? 1 (bytestring-length-code-points (lit "ä"))))
      (assert (fx=? 2 (bytestring-length-code-points (lit "äが"))))
      (assert (fx=? 2 (bytestring-length-code-points (lit "🍔🍔"))))

      ;; ; ;; hash
      ;; ; (assert (fx=? (bytestring-hash (bytestring-slice (lit "foo bar") 4 7))
      ;; ;               (bytestring-hash (lit "bar"))))
      ;; ; (assert (fx=? (bytestring-hash (lit ""))
      ;; ;               (bytestring-hash (lit ""))))
      ;; ; (assert (fx=? (bytestring-hash (lit "foo"))
      ;; ;               (bytestring-hash (lit "foo"))))
      ;; ; (assert (not (fx=? (bytestring-hash (lit "foo"))
      ;; ;                    (bytestring-hash (lit "bar")))))

      ;; slicing
      (assert (string=?
                (bytestring->string (bytestring-slice (lit "foo") 0))
                "foo"))
      (assert (string=?
                (bytestring->string (bytestring-slice (lit "foo") 1))
                "oo"))
      (assert (string=?
                (bytestring->string (bytestring-slice (lit "foo bar") 4 7))
                "bar"))
      (assert (string=?
                (bytestring->string (bytestring-slice (lit "foo bar") 4 100))
                "bar"))
      (assert (string=?
                (bytestring->string (bytestring-slice (lit "foo") -1))
                "foo"))

      ;; take
      (assert (bytestring=? (bytestring-take (lit "foo") -1) (lit "")))
      (assert (bytestring=? (bytestring-take (lit "foo") 0) (lit "")))
      (assert (bytestring=? (bytestring-take (lit "foo") 1) (lit "f")))
      (assert (bytestring=? (bytestring-take (lit "foo") 2) (lit "fo")))
      (assert (bytestring=? (bytestring-take (lit "foo") -3) (lit "")))
      (assert (bytestring=? (bytestring-take (lit "foo") 10) (lit "foo")))

      ;; drop
      (assert (bytestring=? (bytestring-drop (lit "foo") -1) (lit "foo")))
      (assert (bytestring=? (bytestring-drop (lit "foo") 0) (lit "foo")))
      (assert (bytestring=? (bytestring-drop (lit "foo") 1) (lit "oo")))
      (assert (bytestring=? (bytestring-drop (lit "foo") 2) (lit "o")))
      (assert (bytestring=? (bytestring-drop (lit "foo") 10) (lit "")))
      (assert (bytestring=? (bytestring-take (bytestring-drop (lit "foo bar baz") 4) 3) (lit "bar")))

      ;; index-of
      (assert (fx=? 0 (bytestring-index-of (lit "foo bar") (lit ""))))
      (assert (fx=? 0 (bytestring-index-of (lit "") (lit ""))))
      (assert (not (bytestring-index-of (lit "foo bar") (lit "baz"))))
      (assert (fx=? 0 (bytestring-index-of (lit "foo bar") (lit "f"))))
      (assert (fx=? 4 (bytestring-index-of (lit "foo bar") (lit "b"))))
      (assert (fx=? 4 (bytestring-index-of (lit "foo bar") (lit "bar"))))
      (assert (fx=? 3 (bytestring-index-of (lit "fo foo") (lit "foo"))))
      (assert (fx=? 1 (bytestring-index-of (lit "zza") (lit "za"))))
      (assert (fx=? 0 (bytestring-index-of (lit "𝕒𝕓𝕔") (lit "𝕒"))))
      (assert (fx=? 2 (bytestring-index-of (lit "𝕒𝕓𝕔") (lit "𝕓"))))

      ;; last-index-of
      (assert (fx=? 7 (bytestring-last-index-of (lit "foo bar") (lit ""))))
      (assert (fx=? 0 (bytestring-last-index-of (lit "") (lit ""))))
      (assert (not (bytestring-index-of (lit "foo bar") (lit "baz"))))
      (assert (fx=? 8 (bytestring-last-index-of (lit "foo bar foo") (lit "foo"))))

      ;; comparisons
      (assert (bytestring=? (lit "abc") (lit "abc")))
      (assert (bytestring=? (lit "𝕒𝕓𝕔") (lit "𝕒𝕓𝕔")))
      (assert (bytestring<? (lit "fo") (lit "foo")))
      (assert (bytestring<? (lit "a") (lit "z")))
      (assert (bytestring<? (lit "az") (lit "z")))
      (assert (not (bytestring<? (lit "foreign") (lit "else"))))
      (assert (not (bytestring<? (lit "") (lit ""))))
      (assert (not (bytestring>? (lit "a") (lit "a"))))
      (assert (not (bytestring>? (lit "a") (lit "aa"))))
      (assert (not (bytestring>? (lit "") (lit ""))))
      (assert (not (bytestring>? (lit "else") (lit "foreign"))))
      (assert (bytestring>=? (lit "") (lit "")))
      (assert (bytestring>=? (lit "zoo") (lit "zoo")))
      (assert (bytestring>=? (lit "zoo") (lit "zo")))
      (assert (not (bytestring>=? (lit "zo") (lit "zoo"))))
      (assert (let ([s (lit "foo")]) (bytestring>=? s s)))

      (assert (bytestring<=? (lit "") (lit "")))
      (assert (not (bytestring<=? (lit "zoo") (lit "zo"))))
      (assert (bytestring<=? (lit "zo") (lit "zoo")))
      (assert (let ([s (lit "foo")]) (bytestring<=? s s)))

      ;; casing
      (assert (bytestring=? (bytestring-downcase (lit "")) (lit "")))
      (assert (bytestring=? (bytestring-downcase (lit "foo")) (lit "foo")))
      (assert (bytestring=? (bytestring-downcase (lit "FOO")) (lit "foo")))
      (assert (bytestring=? (bytestring-upcase (lit "")) (lit "")))
      (assert (bytestring=? (bytestring-upcase (lit "FOO")) (lit "FOO")))
      (assert (bytestring=? (bytestring-upcase (lit "foo")) (lit "FOO")))
      (assert (not (bytestring=? (bytestring-upcase (lit "foo")) (lit "FOo"))))

      (assert (bytestring=?
                (char-flexvector->bytestring (srfi:214:flexvector))
                (lit "")))
      (assert (bytestring=?
                (char-flexvector->bytestring (srfi:214:flexvector #\f #\o #\o))
                (lit "foo")))

      (assert (srfi:214:flexvector=?
                equal?
                (bytestring->char-flexvector (lit ""))
                (srfi:214:flexvector)))
      (assert (srfi:214:flexvector=?
                equal?
                (bytestring->char-flexvector (lit "a"))
                (srfi:214:flexvector #\a)))
      (assert (srfi:214:flexvector=?
                equal?
                (bytestring->char-flexvector (lit "foo"))
                (srfi:214:flexvector #\f #\o #\o)))

      (assert (bytestring=? (replace "" "bar" "foo") (lit "")))
      (assert (bytestring=? (replace "foo" "foo" "bar") (lit "bar")))
      (assert (bytestring=? (replace "foo" "" "bar") (lit "foo")))
      (assert (bytestring=? (replace "javascript" "java" "pure") (lit "purescript")))
      (assert (bytestring=? (replace "foo bar" " bar" "") (lit "foo")))
      (assert (bytestring=? (replace "𝕒𝕓𝕔" "𝕒" "a") (lit "a𝕓𝕔")))
      (assert (bytestring=? (replace "abc" "b" "") (lit "ac")))
      (assert (bytestring=? (replace "𝕒𝕓𝕔" "𝕓" "") (lit "𝕒𝕔")))
      (assert (bytestring=? (replace "𝕒𝕓𝕔" "𝕔" "") (lit "𝕒𝕓")))
      (assert (bytestring=? (replace "𝕒𝕓𝕔" "𝕒" "") (lit "𝕓𝕔")))

      (assert (bytestring=? (replace-all "" "" "") (lit "")))
      (assert (bytestring=? (replace-all "aaa" "a" "b") (lit "bbb")))
      (assert (bytestring=? (replace-all "aaac" "a" "b") (lit "bbbc")))
      (assert (bytestring=? (replace-all "foo foo foo" "foo" "bar") (lit "bar bar bar")))
      (assert (bytestring=? (replace-all "abbc" "b" "") (lit "ac")))
      (assert (bytestring=? (replace-all "abbbbbc" "b" "") (lit "ac")))
      (assert (bytestring=? (replace-all "𝕒𝕓𝕓𝕔" "𝕓" "") (lit "𝕒𝕔")))
      (assert (bytestring=? (replace-all "𝕒𝕓𝕓𝕔" "𝕓𝕓" "b") (lit "𝕒b𝕔")))
      (assert (bytestring=? (bytestring-replace-all
                              (bytestring-drop (lit "foo 𝕒𝕓𝕓𝕔") 4)
                              (lit "𝕓𝕓")
                              (lit "b"))
                            (lit "𝕒b𝕔")))

      (assert (srfi:214:flexvector=? bytestring=? (split "" "a") (srfi:214:flexvector)))
      ;; (assert (srfi:214:flexvector=? bytestring=? (split "ab" "") (srfi:214:flexvector (lit "a") (lit "b"))))
      ;; (assert (srfi:214:flexvector=? bytestring=? (split "aabcc" "b") (srfi:214:flexvector (lit "aa") (lit "cc"))))

      (assert (bytestring=? (bytestring-trim (lit "   ")) (lit "")))
      (assert (bytestring=? (bytestring-trim (lit " a  ")) (lit "a")))
      (assert (bytestring=? (bytestring-trim (lit " ab ")) (lit "ab")))

      (assert (bytestring=? (bytestring-join-with (srfi:214:flexvector) (lit ",")) (lit "")))
      (assert (bytestring=? (bytestring-join-with (srfi:214:flexvector (lit "a")) (lit ",")) (lit "a")))
      (assert (bytestring=? (bytestring-join-with (srfi:214:flexvector (lit "a") (lit "b")) (lit ",")) (lit "a,b")))
      (assert (bytestring=? (bytestring-join-with (srfi:214:flexvector (lit "abc") (lit "def")) (lit " ")) (lit "abc def")))
      (assert (bytestring=? (bytestring-join-with (srfi:214:flexvector (lit "𝕒") (lit "𝕔")) (lit "𝕓")) (lit "𝕒𝕓𝕔")))


      ;; ; Code points

      (assert (fx=? 7 (bytestring-length-code-points
                        (code-points->bytestring #x61 #xDC00 #xD800 #xD800 #x16805 #x16A06 #x7A))))
      (assert (fx=? #x61
                    (bytestring-ref-code-point
                      (code-points->bytestring #x61 #xDC00 #xD800 #xD800 #x16805 #x16A06 #x7A) 0)))
      (assert (fx=? #xDC00
                    (bytestring-ref-code-point
                      (code-points->bytestring #x61 #xDC00 #xD800 #xD800 #x16805 #x16A06 #x7A) 1)))
      (assert (fx=? #xD800
                    (bytestring-ref-code-point
                      (code-points->bytestring #x61 #xDC00 #xD800 #xD800 #x16805 #x16A06 #x7A) 2)))
      (assert (fx=? #xD800
                    (bytestring-ref-code-point
                      (code-points->bytestring #x61 #xDC00 #xD800 #xD800 #x16805 #x16A06 #x7A) 3)))
      (assert (fx=? #x16805
                    (bytestring-ref-code-point
                      (code-points->bytestring #x61 #xDC00 #xD800 #xD800 #x16805 #x16A06 #x7A) 4)))
      (assert (fx=? #x16A06
                    (bytestring-ref-code-point
                      (code-points->bytestring #x61 #xDC00 #xD800 #xD800 #x16805 #x16A06 #x7A) 5)))
      (assert (fx=? #x7A
                    (bytestring-ref-code-point
                      (code-points->bytestring #x61 #xDC00 #xD800 #xD800 #x16805 #x16A06 #x7A) 6)))


      (assert (bytestring=? (bytestring-take-code-points (lit "foo") 0) (lit "")))
      (assert (bytestring=? (bytestring-take-code-points (lit "foo") -1) (lit "")))
      (assert (bytestring=? (bytestring-take-code-points (lit "") 2) (lit "")))
      (assert (bytestring=? (bytestring-take-code-points (lit "abc") 2) (lit "ab")))
      (assert (bytestring=? (bytestring-take-code-points (lit "abc") 3) (lit "abc")))
      (assert (bytestring=? (bytestring-take-code-points (lit "𝕒𝕓𝕔") 2) (lit "𝕒𝕓")))
      (assert (bytestring=? (bytestring-take-code-points (lit "𝕒𝕓𝕔") 3) (lit "𝕒𝕓𝕔")))
      (assert (bytestring=? (bytestring-take-code-points (lit "𝕒𝕓𝕔") 4) (lit "𝕒𝕓𝕔")))

      ;; regex
      (assert (srfi:214:flexvector=?
                bytestring=?
                (bytestring-regex-match (bytestring-make-regex (lit "foo")) (lit "foo"))
                (srfi:214:flexvector (lit "foo"))))
      (assert (not (bytestring-regex-match (bytestring-make-regex (lit "foo")) (lit "bar"))))
      (assert (srfi:214:flexvector=?
                bytestring=?
                (bytestring-regex-match (bytestring-make-regex (lit "(a)(b)(c)")) (lit "abc"))
                (srfi:214:flexvector (lit "abc") (lit "a") (lit "b") (lit "c"))))
      (assert (srfi:214:flexvector=?
                bytestring=?
                (bytestring-regex-match (bytestring-make-regex (lit "(a|b)(c)")) (lit "ac"))
                (srfi:214:flexvector (lit "ac") (lit "a") (lit "c"))))
      (assert (srfi:214:flexvector=?
                bytestring=?
                (bytestring-regex-match (bytestring-make-regex (lit "(a|b)(c)")) (lit "bc"))
                (srfi:214:flexvector (lit "bc") (lit "b") (lit "c"))))
      (assert (srfi:214:flexvector=?
                bytestring=?
                (bytestring-regex-match (bytestring-make-regex (string->bytestring "^[a-z]+$")) (lit "abc"))
                (srfi:214:flexvector (lit "abc"))))
      (assert (srfi:214:flexvector=?
                bytestring=?
                (bytestring-regex-match (bytestring-make-regex (lit "^[a-z]+$")) (lit "abc"))
                (srfi:214:flexvector (lit "abc"))))
      (assert (not (srfi:214:flexvector-ref
                     (bytestring-regex-match (bytestring-make-regex (lit "(a|(b))|(c)")) (lit "ac"))
                     2)))

      (display "All good!\n")
      ))
  )
