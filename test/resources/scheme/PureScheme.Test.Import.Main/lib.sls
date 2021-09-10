(library
  (PureScheme.Test.Import.Main lib)
  (export main)
  (import
    (rnrs)
    (prefix (Data.Semiring lib) Data.Semiring.)
    (prefix (Prelude lib) Prelude.)
    (prefix (PureScheme.Test.Import.Bar lib) PureScheme.Test.Import.Bar.)
    (prefix (PureScheme.Test.Import.Baz lib) PureScheme.Test.Import.Baz.)
    (prefix (PureScheme.Test.Import.Foo lib) PureScheme.Test.Import.Foo.))


  (define
    main
    (+
      (+ PureScheme.Test.Import.Foo.foo PureScheme.Test.Import.Bar.bar)
      PureScheme.Test.Import.Baz.baz))
  )