(library
  (PureScheme.Test.ReExport.Export lib)
  (export)
  (import
    (prefix (rnrs) scm:)
    (prefix (PureScheme.Test.ReExport.Bar lib) PureScheme.Test.ReExport.Bar.)
    (prefix (PureScheme.Test.ReExport.Foo lib) PureScheme.Test.ReExport.Foo.))

  )