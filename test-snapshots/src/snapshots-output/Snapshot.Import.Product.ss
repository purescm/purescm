#!r6rs
#!chezscheme
(library
  (Snapshot.Import.Product lib)
  (export
    Product
    Product*
    Product-value0
    Product-value1
    Product?)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime lib) rt:))

  (scm:define-record-type (Product$ Product* Product?)
    (scm:fields (scm:immutable value0 Product-value0) (scm:immutable value1 Product-value1)))

  (scm:define Product
    (scm:lambda (value0)
      (scm:lambda (value1)
        (Product* value0 value1)))))
