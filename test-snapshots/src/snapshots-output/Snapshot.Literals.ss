#!r6rs
#!chezscheme
(library
  (Snapshot.Literals lib)
  (export
    array
    array2
    boolean1
    boolean2
    char
    int
    number
    record
    record2
    string)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime) rt:))

  (scm:define string
    (rt:string->bytestring "string"))

  (scm:define record2
    (scm:list (scm:cons (scm:string->symbol "foo") "bar")))

  (scm:define record
    (scm:list))

  (scm:define number
    2.0)

  (scm:define int
    1)

  (scm:define char
    #\a)

  (scm:define boolean2
    #f)

  (scm:define boolean1
    #t)

  (scm:define array2
    (rt:make-array 1 2 3))

  (scm:define array
    (rt:make-array)))
