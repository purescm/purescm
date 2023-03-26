#!r6rs
#!chezscheme
(library
  (Snapshot.Constructor lib)
  (export
    Nil*
    Nil?
    Node
    Node*
    Node?
    Node$-value0
    Node$-value1
    Node$-value2
    Node
    Just*
    Just?
    Just$-value0
    Nothing*
    Nothing?)
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
    (scm:fields)))
