#!r6rs
#!chezscheme
(library
  (Snapshot.ArrayIndex lib)
  (export
    testAccessorGetIndex)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime) rt:))

  (scm:define testAccessorGetIndex
    (scm:lambda (v0)
      (scm:cond
        [(scm:fx=? (rt:array-length v0) 1) (rt:array-ref v0 0)]
        [scm:else 0]))))
