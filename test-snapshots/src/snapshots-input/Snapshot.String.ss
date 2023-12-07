(library (Snapshot.String foreign)
  (export testStringMain)
  (import
    (chezscheme)
    (only (purs runtime bytestring-test) main))

  (define testStringMain main)

  )

