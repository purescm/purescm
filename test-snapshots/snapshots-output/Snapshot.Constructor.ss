#!r6rs
#!chezscheme
(library
  (Snapshot.Constructor lib)
  (export
    Just*
    Just?
    Nil*
    Nil?
    Node
    Node*
    Node?
    Nothing*
    Nothing?
    extractInt)
  (import
    (prefix (chezscheme) scm:)
    (prefix (_Chez_Runtime lib) rt:))

  (scm:define-record-type (Nil$ Nil* Nil?)
    (scm:fields))

  (scm:define-record-type (Node$ Node* Node?)
    (scm:fields value0 value1 value2))

  (scm:define Node
    (scm:lambda (value0)
      (scm:lambda (value1)
        (scm:lambda (value2)
          (Node* value0 value1 value2)))))

  (scm:define-record-type (Just$ Just* Just?)
    (scm:fields value0))

  (scm:define-record-type (Nothing$ Nothing* Nothing?)
    (scm:fields))

  (scm:define extractInt
    (scm:lambda (v0)
      (scm:cond ((Just? v0) (scm:letrec* (($record v0)) ((scm:record-accessor (scm:record-rtd $record) 0) $record))) ((Nothing? v0) 0) (scm:else (scm:raise (scm:condition (scm:make-error) (scm:make-message-condition "Failed pattern match"))))))))
