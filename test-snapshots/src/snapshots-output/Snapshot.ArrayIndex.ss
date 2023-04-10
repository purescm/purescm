#!r6rs
#!chezscheme
(library
  (Snapshot.ArrayIndex lib)
  (export
    testAccessorGetIndex)
  (import
    (prefix (chezscheme) scm:)
    (prefix (_Chez_Runtime lib) rt:))

  (scm:define testAccessorGetIndex
    (scm:lambda (v0)
      (scm:if (scm:fx=? (scm:vector-length v0) 1)
              (scm:vector-ref v0 0)
              0))))
