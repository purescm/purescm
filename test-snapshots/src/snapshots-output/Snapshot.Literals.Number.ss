#!r6rs
#!chezscheme
(library
  (Snapshot.Literals.Number lib)
  (export
    minusInfinity
    nan
    plusInfinity)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime) rt:))

  (scm:define plusInfinity
    +inf.0)

  (scm:define nan
    +nan.0)

  (scm:define minusInfinity
    -inf.0))
