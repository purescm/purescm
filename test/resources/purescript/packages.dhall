let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20210826/packages.dhall sha256:eee0765aa98e0da8fc414768870ad588e7cada060f9f7c23c37385c169f74d9f
in  upstream
    with prelude.repo = "https://github.com/purescm/purescript-prelude.git"
    with prelude.version = "65487a3ee46947523cd5d6311d0d2dc06f15aa59"

    with effect.repo = "https://github.com/purescm/purescript-effect.git"
    with effect.version = "9267ec66ca12f4ae48955e1159cf303612487bd1"

    with console.repo = "https://github.com/purescm/purescript-console.git"
    with console.version = "6ae590ecb264ce8aba4a6164a15b62ac49c119c5"