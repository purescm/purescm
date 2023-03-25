#!r6rs
#!chezscheme
(library
  (Snapshot.Constructor lib)
  (export
    Just
    Nil
    Node
    Nothing)
  (import
    (prefix (chezscheme) scm:)
    (prefix (_Chez_Runtime lib) rt:))

  (scm:define-record-type (Nil Nil* Nil?)
    (scm:fields))

  (scm:define-record-type (Node Node* Node?)
    (scm:fields value0 value1 value2))

  (scm:define-record-type (Just Just* Just?)
    (scm:fields value0))

  (scm:define-record-type (Nothing Nothing* Nothing?)
    (scm:fields)))
