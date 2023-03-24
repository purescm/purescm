#!r6rs
#!chezscheme
(library
  (Snapshot.Effect lib)
  (export
    dontInlineMe
    main)
  (import
    (prefix (chezscheme) scm:)
    (prefix (_Chez_Runtime lib) rt:)
    (prefix (Data.Unit lib) Data.Unit.))

  (scm:define dontInlineMe
    (scm:lambda (v0)
      (scm:lambda () Data.Unit.unit)))

  (scm:define main
    (scm:let* ((_0 (dontInlineMe "a"))) (scm:lambda () (scm:let* (($__unused1 (_0)) ($__unused2 ((dontInlineMe "b"))) (value3 ((dontInlineMe "c"))) ($__unused4 ((dontInlineMe "d"))) ($__unused5 ((dontInlineMe "e")))) value3)))))
