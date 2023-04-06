#!r6rs
#!chezscheme
(library
  (Snapshot.ConstructorAccessor lib)
  (export
    First
    First-value0
    First?
    Foo
    Foo*
    Foo-value0
    Foo-value1
    Foo-value2
    Foo?
    HasArgs
    HasArgs*
    HasArgs-value0
    HasArgs-value1
    HasArgs-value2
    HasArgs?
    Last
    Last-value0
    Last?
    NoArgs
    NoArgs?
    test1
    test2
    test3
    test4
    test5
    x
    y
    z)
  (import
    (prefix (chezscheme) scm:)
    (prefix (_Chez_Runtime lib) rt:))

  (scm:define-record-type (First$ First First?)
    (scm:fields (scm:immutable value0 First-value0)))

  (scm:define-record-type (Last$ Last Last?)
    (scm:fields (scm:immutable value0 Last-value0)))

  (scm:define NoArgs
    (scm:quote NoArgs))

  (scm:define NoArgs?
    (scm:lambda (v)
      (scm:eq? (scm:quote NoArgs) v)))

  (scm:define-record-type (HasArgs$ HasArgs* HasArgs?)
    (scm:fields (scm:immutable value0 HasArgs-value0) (scm:immutable value1 HasArgs-value1) (scm:immutable value2 HasArgs-value2)))

  (scm:define HasArgs
    (scm:lambda (value0)
      (scm:lambda (value1)
        (scm:lambda (value2)
          (HasArgs* value0 value1 value2)))))

  (scm:define-record-type (Foo$ Foo* Foo?)
    (scm:fields (scm:immutable value0 Foo-value0) (scm:immutable value1 Foo-value1) (scm:immutable value2 Foo-value2)))

  (scm:define Foo
    (scm:lambda (value0)
      (scm:lambda (value1)
        (scm:lambda (value2)
          (Foo* value0 value1 value2)))))

  (scm:define z
    (Foo* 1 1 1))

  (scm:define y
    ((Foo 1) 1))

  (scm:define x
    (Foo 1))

  (scm:define test5
    (scm:lambda (_)
      (scm:lambda (v1)
        (scm:cond
          [(First? v1) (First-value0 v1)]
          [scm:else (scm:raise (scm:condition (scm:make-error) (scm:make-message-condition "Failed pattern match")))]))))

  (scm:define test4
    (scm:lambda (v0)
      (scm:cond
        [(First? v0) (First-value0 v0)]
        [(Last? v0) (Last-value0 v0)]
        [scm:else (scm:raise (scm:condition (scm:make-error) (scm:make-message-condition "Failed pattern match")))])))

  (scm:define test3
    (scm:lambda (v0)
      (scm:cond
        [(scm:fx<? (HasArgs-value0 v0) (HasArgs-value2 v0)) (HasArgs-value0 v0)]
        [scm:else (HasArgs-value1 v0)])))

  (scm:define test2
    (scm:lambda (v0)
      (HasArgs-value0 v0)))

  (scm:define test1
    (scm:lambda (v0)
      #t)))
