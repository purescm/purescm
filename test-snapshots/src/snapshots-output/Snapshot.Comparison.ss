#!r6rs
#!chezscheme
(library
  (Snapshot.Comparison lib)
  (export
    booleanComparison
    charComparison
    integerComparison
    numberComparison
    stringComparison)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime) rt:))

  (scm:define stringComparison
    (scm:lambda (x0)
      (scm:lambda (y1)
        (rt:make-array (rt:pstring=? x0 y1) (scm:not (rt:pstring=? x0 y1)) (rt:pstring<? x0 y1) (rt:pstring<=? x0 y1) (rt:pstring>=? x0 y1) (rt:pstring>? x0 y1)))))

  (scm:define numberComparison
    (scm:lambda (x0)
      (scm:lambda (y1)
        (rt:make-array (scm:fl=? x0 y1) (scm:not (scm:fl=? x0 y1)) (scm:fl<? x0 y1) (scm:fl<=? x0 y1) (scm:fl>=? x0 y1) (scm:fl>? x0 y1)))))

  (scm:define integerComparison
    (scm:lambda (x0)
      (scm:lambda (y1)
        (rt:make-array (scm:fx=? x0 y1) (scm:not (scm:fx=? x0 y1)) (scm:fx<? x0 y1) (scm:fx<=? x0 y1) (scm:fx>=? x0 y1) (scm:fx>? x0 y1)))))

  (scm:define charComparison
    (scm:lambda (x0)
      (scm:lambda (y1)
        (rt:make-array (scm:char=? x0 y1) (scm:not (scm:char=? x0 y1)) (scm:char<? x0 y1) (scm:char<=? x0 y1) (scm:char>=? x0 y1) (scm:char>? x0 y1)))))

  (scm:define booleanComparison
    (scm:lambda (x0)
      (scm:lambda (y1)
        (rt:make-array (scm:boolean=? x0 y1) (scm:not (scm:boolean=? x0 y1)) (rt:boolean<? x0 y1) (rt:boolean<=? x0 y1) (rt:boolean>=? x0 y1) (rt:boolean>? x0 y1))))))
