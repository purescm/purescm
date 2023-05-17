#!r6rs
#!chezscheme
(library
  (Snapshot.Import.Impl lib)
  (export
    addImpl
    fortyTwo)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime lib) rt:)
    (Snapshot.Import.Impl foreign))

  (scm:define fortyTwo
    ((addImpl 21) 21)))
