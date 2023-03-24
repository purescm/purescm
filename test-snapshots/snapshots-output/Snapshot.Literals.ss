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
    (prefix (_Chez_Runtime lib) rt:))

  (scm:define string
    "string")

  (scm:define record2
    (scm:letrec* (($record (scm:make-hashtable scm:string-hash scm:string=?))) (scm:hashtable-set! $record "foo" "bar") $record))

  (scm:define record
    (scm:letrec* (($record (scm:make-hashtable scm:string-hash scm:string=?))) $record))

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
    (scm:vector 1 2 3))

  (scm:define array
    (scm:vector)))
