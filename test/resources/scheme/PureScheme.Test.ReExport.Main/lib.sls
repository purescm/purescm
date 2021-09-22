(library
  (PureScheme.Test.ReExport.Main lib)
  (export main)
  (import
    (prefix (rnrs) scm:)
    (prefix (Data.Semiring lib) Data.Semiring.)
    (prefix (Prelude lib) Prelude.)
    (prefix (PureScheme.Test.ReExport.Bar lib) PureScheme.Test.ReExport.Bar.)
    (prefix
      (PureScheme.Test.ReExport.Export lib)
      PureScheme.Test.ReExport.Export.)
    (prefix (PureScheme.Test.ReExport.Foo lib) PureScheme.Test.ReExport.Foo.))


  (scm:define
    main
    (scm:fx+ PureScheme.Test.ReExport.Foo.foo PureScheme.Test.ReExport.Bar.bar))
  )