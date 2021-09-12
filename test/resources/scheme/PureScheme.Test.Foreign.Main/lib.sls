(library
  (PureScheme.Test.Foreign.Main lib)
  (export main)
  (import
    (prefix (rnrs) scm:)
    (prefix
      (PureScheme.Test.Foreign.Constant lib)
      PureScheme.Test.Foreign.Constant.)
    (prefix
      (PureScheme.Test.Foreign.Function lib)
      PureScheme.Test.Foreign.Function.))


  (scm:define
    main
    (PureScheme.Test.Foreign.Function.inc PureScheme.Test.Foreign.Constant.one))
  )