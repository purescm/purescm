#!r6rs
#!chezscheme
(library
  (Snapshot.Constructor lib)
  (export
    Just$-value0
    Just*
    Just?
    Nil
    Nil?
    Node
    Node$-value0
    Node$-value1
    Node$-value2
    Node*
    Node?
    Nothing
    Nothing?)
  (import
    (prefix (chezscheme) scm:)
    (prefix (_Chez_Runtime lib) rt:))

  (scm:define Nil
    (scm:quote Nil))

  (scm:define Nil?
    (scm:lambda (v)
      (scm:eq? (scm:quote Nil) v)))

  (scm:define-record-type (Node$ Node* Node?)
    (scm:fields value0 value1 value2))

  (scm:define Node
    (scm:lambda (value0)
      (scm:lambda (value1)
        (scm:lambda (value2)
          (Node* value0 value1 value2)))))

  (scm:define-record-type (Just$ Just* Just?)
    (scm:fields value0))

  (scm:define Nothing
    (scm:quote Nothing))

  (scm:define Nothing?
    (scm:lambda (v)
      (scm:eq? (scm:quote Nothing) v))))
