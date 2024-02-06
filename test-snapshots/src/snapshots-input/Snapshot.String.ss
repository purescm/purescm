(library (Snapshot.String foreign)
  (export testStringMain)
  (import
    (chezscheme)
    (only (purs runtime pstring-test) main))

  (define testStringMain main)

  )

