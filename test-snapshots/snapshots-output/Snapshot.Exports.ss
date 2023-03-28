#!r6rs
#!chezscheme
(library
  (Snapshot.Exports lib)
  (export
    TypeAndCtor
    TypeAndCtor?)
  (import
    (prefix (chezscheme) scm:)
    (prefix (_Chez_Runtime lib) rt:))

  (scm:define TypeAndCtor
    (scm:quote TypeAndCtor))

  (scm:define TypeAndCtor?
    (scm:lambda (v)
      (scm:eq? (scm:quote TypeAndCtor) v))))
