#!r6rs
#!chezscheme
(library
  (Snapshot.Import lib)
  (export
    fortyThree)
  (import
    (prefix (chezscheme) scm:)
    (prefix (_Chez_Runtime lib) rt:)
    (prefix (Snapshot.Import.Impl lib) Snapshot.Import.Impl.))

  (scm:define fortyThree
    ((Snapshot.Import.Impl.addImpl 21) 22)))
