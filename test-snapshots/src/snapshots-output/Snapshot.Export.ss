#!r6rs
#!chezscheme
(library
  (Snapshot.Export lib)
  (export
    exported
    exportedForeign)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime lib) rt:)
    (Snapshot.Export foreign))

  (scm:define-record-type (Baz$ Baz* Baz?)
    (scm:fields (scm:immutable value0 Baz-value0) (scm:immutable value1 Baz-value1)))

  (scm:define Baz
    (scm:lambda (value0)
      (scm:lambda (value1)
        (Baz* value0 value1))))

  (scm:define exported
    (Baz* 1 2)))
