#!r6rs
#!chezscheme
(library
  (Snapshot.Import lib)
  (export
    foo
    fortyThree
    fst)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime) rt:)
    (prefix (Snapshot.Import.Constructor lib) Snapshot.Import.Constructor.)
    (prefix (Snapshot.Import.Impl lib) Snapshot.Import.Impl.)
    (prefix (Snapshot.Import.Product lib) Snapshot.Import.Product.))

  (scm:define fst
    (scm:lambda (v0)
      (Snapshot.Import.Product.Product-value0 v0)))

  (scm:define fortyThree
    ((Snapshot.Import.Impl.addImpl 21) 22))

  (scm:define foo
    Snapshot.Import.Constructor.Foo))
