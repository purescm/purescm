#!r6rs
#!chezscheme
(library
  (Snapshot.Import.Constructor lib)
  (export
    Foo
    Foo?)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime) rt:))

  (scm:define Foo
    (scm:quote Foo))

  (scm:define Foo?
    (scm:lambda (v)
      (scm:eq? (scm:quote Foo) v))))
