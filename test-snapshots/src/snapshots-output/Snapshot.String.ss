#!r6rs
#!chezscheme
(library
  (Snapshot.String lib)
  (export
    main
    testStringMain)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime lib) rt:)
    (Snapshot.String foreign))

  (scm:define main
    testStringMain))
