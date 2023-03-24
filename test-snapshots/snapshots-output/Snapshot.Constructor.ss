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

  (scm:define Nil
    (scm:lambda ()
      (scm:cons (scm:quote Nil) scm:nil)))

  (scm:define Node
    (scm:lambda (value0)
      (scm:lambda (value1)
        (scm:lambda (value2)
          (scm:cons (scm:quote Node) (scm:vector value0 value1 value2))))))

  (scm:define Just
    (scm:lambda (value0)
      (scm:cons (scm:quote Just) (scm:vector value0))))

  (scm:define Nothing
    (scm:lambda ()
      (scm:cons (scm:quote Nothing) scm:nil))))
