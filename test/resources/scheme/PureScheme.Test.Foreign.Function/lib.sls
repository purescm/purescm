(library
  (PureScheme.Test.Foreign.Function lib)
  (export inc)
  (import
    (prefix (rnrs) scm:)
    (only (PureScheme.Test.Foreign.Function foreign) inc))

  )