#!r6rs
#!chezscheme
(library
  (Snapshot.If lib)
  (export
    T
    T-value0
    T?
    U
    U-value0
    U?
    ifThen
    performWhen
    t)
  (import
    (prefix (chezscheme) scm:)
    (prefix (_Chez_Runtime lib) rt:)
    (prefix (Data.Unit lib) Data.Unit.)
    (prefix (Effect.Console lib) Effect.Console.))

  (scm:define-record-type (T$ T T?)
    (scm:fields (scm:immutable value0 T-value0)))

  (scm:define-record-type (U$ U U?)
    (scm:fields (scm:immutable value0 U-value0)))

  (scm:define t
    (scm:lambda (_)
      (scm:lambda (v1)
        (scm:if (T? v1)
                (T-value0 v1)
                (scm:raise (scm:condition (scm:make-error) (scm:make-message-condition "Failed pattern match")))))))

  (scm:define performWhen
    (scm:lambda (condition0)
      (scm:let ([_1 (Effect.Console.log "performWhen")])
        (scm:if condition0
                _1
                (scm:lambda ()
                  Data.Unit.unit)))))

  (scm:define ifThen
    (scm:lambda (v0)
      (scm:if v0
              0
              1))))
