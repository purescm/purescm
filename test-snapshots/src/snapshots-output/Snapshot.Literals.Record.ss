#!r6rs
#!chezscheme
(library
  (Snapshot.Literals.Record lib)
  (export
    recordAccess)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime lib) rt:))

  (scm:define recordAccess
    (scm:lambda (v0)
      (rt:object-ref v0 "fooBarBaz"))))
