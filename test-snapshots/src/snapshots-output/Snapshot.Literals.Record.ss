#!r6rs
#!chezscheme
(library
  (Snapshot.Literals.Record lib)
  (export
    recordAccess)
  (import
    (prefix (chezscheme) scm:)
    (prefix (_Chez_Runtime lib) rt:))

  (scm:define recordAccess
    (scm:lambda (v0)
      (scm:hashtable-ref v0 "fooBarBaz" #f))))
