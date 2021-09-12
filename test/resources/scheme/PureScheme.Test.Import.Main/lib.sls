(library
  (PureScheme.Test.Import.Main lib)
  (export main)
  (import
    (prefix (rnrs) scm:)
    (prefix (Data.Semiring lib) Data.Semiring.)
    (prefix (Prelude lib) Prelude.)
    (prefix (PureScheme.Test.Import.Bar lib) PureScheme.Test.Import.Bar.)
    (prefix (PureScheme.Test.Import.Baz lib) PureScheme.Test.Import.Baz.)
    (prefix (PureScheme.Test.Import.Foo lib) PureScheme.Test.Import.Foo.))


  (scm:define
    main
    (scm:+
      (scm:+ PureScheme.Test.Import.Foo.foo PureScheme.Test.Import.Bar.bar)
      PureScheme.Test.Import.Baz.baz))
  )