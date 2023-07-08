#!r6rs
#!chezscheme
(library
  (Snapshot.Export lib)
  (export
    bar
    exported
    exportedForeign
    foo)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime lib) rt:)
    (Snapshot.Export foreign))

  (scm:define-record-type (Two$ Two* Two?)
    (scm:fields (scm:immutable value0 Two-value0) (scm:immutable value1 Two-value1)))

  (scm:define Two
    (scm:lambda (value0)
      (scm:lambda (value1)
        (Two* value0 value1))))

  (scm:define Foo
    (scm:quote Foo))

  (scm:define Foo?
    (scm:lambda (v)
      (scm:eq? (scm:quote Foo) v)))

  (scm:define-record-type (Bar$ Bar* Bar?)
    (scm:fields (scm:immutable value0 Bar-value0) (scm:immutable value1 Bar-value1)))

  (scm:define Bar
    (scm:lambda (value0)
      (scm:lambda (value1)
        (Bar* value0 value1))))

  (scm:define foo
    Foo)

  (scm:define exported
    (Two* 1 2))

  (scm:define bar
    (Bar* 1 2)))
