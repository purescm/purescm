#!r6rs
#!chezscheme
(library
  (Snapshot.Import.Impl lib)
  (export
    Product
    Product*
    Product-value0
    Product-value1
    Product?
    addImpl
    fortyTwo)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime) rt:)
    (Snapshot.Import.Impl foreign))

  (scm:define-record-type (Product$ Product* Product?)
    (scm:fields (scm:immutable value0 Product-value0) (scm:immutable value1 Product-value1)))

  (scm:define Product
    (scm:lambda (value0)
      (scm:lambda (value1)
        (Product* value0 value1))))

  (scm:define fortyTwo
    ((addImpl 21) 21)))
