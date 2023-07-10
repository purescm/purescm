#!r6rs
#!chezscheme
(library
  (purs runtime lib)
  (export
    object-copy
    object-set!
    object-ref
    make-object
    array-ref
    array-length
    make-array
    boolean<?
    boolean<=?
    boolean>=?
    boolean>?)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime srfi :125) srfi:125:)
    (prefix (purs runtime srfi :128) srfi:128:)
    (prefix (purs runtime srfi :214) srfi:214:))

  (scm:define boolean->integer
    (scm:lambda (x)
      (scm:if x 1 0)))

  (scm:define string-comparator
    (srfi:128:make-comparator scm:string? scm:string=? scm:string<? scm:string-hash))

  (scm:define boolean>?
    (scm:lambda (x)
      (scm:lambda (y)
        (scm:fx>? (boolean->integer x) (boolean->integer y)))))

  (scm:define boolean>=?
    (scm:lambda (x)
      (scm:lambda (y)
        (scm:fx>=? (boolean->integer x) (boolean->integer y)))))

  (scm:define boolean<=?
    (scm:lambda (x)
      (scm:lambda (y)
        (scm:fx<=? (boolean->integer x) (boolean->integer y)))))

  (scm:define boolean<?
    (scm:lambda (x)
      (scm:lambda (y)
        (scm:fx<? (boolean->integer x) (boolean->integer y)))))

  (scm:define make-array
    srfi:214:flexvector)

  (scm:define array-ref
    srfi:214:flexvector-ref)

  (scm:define array-length
    srfi:214:flexvector-length)

  (scm:define make-object
    (scm:lambda args (scm:apply srfi:125:hash-table (scm:cons string-comparator args))))

  (scm:define object-ref
    srfi:125:hash-table-ref)

  (scm:define object-set!
    srfi:125:hash-table-set!)

  (scm:define object-copy
    (scm:lambda (v)
      (srfi:125:hash-table-copy v #t))))
