let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20210826/packages.dhall sha256:eee0765aa98e0da8fc414768870ad588e7cada060f9f7c23c37385c169f74d9f
in  upstream
    with prelude.repo = "https://github.com/purescm/purescript-prelude"
    with prelude.version = "8d5a844b3dcc9a7cad71df1967624e3cdc6703cd"
