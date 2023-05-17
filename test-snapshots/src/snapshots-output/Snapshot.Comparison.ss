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
    (prefix (purs runtime lib) rt:))

  (scm:define stringComparison
    (scm:lambda (x0)
      (scm:lambda (y1)
        (scm:vector (scm:string=? x0 y1) (scm:not (scm:string=? x0 y1)) (scm:string<? x0 y1) (scm:string<=? x0 y1) (scm:string>=? x0 y1) (scm:string>? x0 y1)))))

  (scm:define numberComparison
    (scm:lambda (x0)
      (scm:lambda (y1)
        (scm:vector (scm:fl=? x0 y1) (scm:not (scm:fl=? x0 y1)) (scm:fl<? x0 y1) (scm:fl<=? x0 y1) (scm:fl>=? x0 y1) (scm:fl>? x0 y1)))))

  (scm:define integerComparison
    (scm:lambda (x0)
      (scm:lambda (y1)
        (scm:vector (scm:fx=? x0 y1) (scm:not (scm:fx=? x0 y1)) (scm:fx<? x0 y1) (scm:fx<=? x0 y1) (scm:fx>=? x0 y1) (scm:fx>? x0 y1)))))

  (scm:define charComparison
    (scm:lambda (x0)
      (scm:lambda (y1)
        (scm:vector (scm:char=? x0 y1) (scm:not (scm:char=? x0 y1)) (scm:char<? x0 y1) (scm:char<=? x0 y1) (scm:char>=? x0 y1) (scm:char>? x0 y1)))))

  (scm:define booleanComparison
    (scm:lambda (x0)
      (scm:lambda (y1)
        (scm:vector (scm:boolean=? x0 y1) (scm:not (scm:boolean=? x0 y1)) (rt:boolean<? x0 y1) (rt:boolean<=? x0 y1) (rt:boolean>=? x0 y1) (rt:boolean>? x0 y1))))))
