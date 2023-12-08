#!r6rs
#!chezscheme
(library
  (Snapshot.Constructor lib)
  (export
    Continue1
    Continue1-value0
    Continue1?
    Continue2
    Continue2-value0
    Continue2?
    Just
    Just-value0
    Just?
    Nil
    Nil?
    Node
    Node*
    Node-value0
    Node-value1
    Node-value2
    Node?
    Nothing
    Nothing?
    Stop1
    Stop1?
    Stop2
    Stop2?)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime) rt:))

  (scm:define Nil
    (scm:quote Nil))

  (scm:define Nil?
    (scm:lambda (v)
      (scm:eq? (scm:quote Nil) v)))

  (scm:define-record-type (Node$ Node* Node?)
    (scm:fields (scm:immutable value0 Node-value0) (scm:immutable value1 Node-value1) (scm:immutable value2 Node-value2)))

  (scm:define Node
    (scm:lambda (value0)
      (scm:lambda (value1)
        (scm:lambda (value2)
          (Node* value0 value1 value2)))))

  (scm:define Stop1
    (scm:quote Stop1))

  (scm:define Stop1?
    (scm:lambda (v)
      (scm:eq? (scm:quote Stop1) v)))

  (scm:define-record-type (Continue1$ Continue1 Continue1?)
    (scm:fields (scm:immutable value0 Continue1-value0)))

  (scm:define Stop2
    (scm:quote Stop2))

  (scm:define Stop2?
    (scm:lambda (v)
      (scm:eq? (scm:quote Stop2) v)))

  (scm:define-record-type (Continue2$ Continue2 Continue2?)
    (scm:fields (scm:immutable value0 Continue2-value0)))

  (scm:define-record-type (Just$ Just Just?)
    (scm:fields (scm:immutable value0 Just-value0)))

  (scm:define Nothing
    (scm:quote Nothing))

  (scm:define Nothing?
    (scm:lambda (v)
      (scm:eq? (scm:quote Nothing) v))))
