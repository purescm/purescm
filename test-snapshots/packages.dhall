let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.7-20230320/packages.dhall
        sha256:6f2a4b50b793f304d3a64fd25d631de990de280981c73b0683a090e4fa499f0d

in  upstream
  with prelude =
    { dependencies = [] : List Text
    , repo = "https://github.com/purescm/purescript-prelude.git"
    , version = "v6.0.1-scm"
    }
  with partial =
    { dependencies = [] : List Text
    , repo = "https://github.com/purescm/purescript-partial.git"
    , version = "v4.0.0-scm"
    }
  with effect =
    { dependencies = [ "prelude" ]
    , repo = "https://github.com/purescm/purescript-effect.git"
    , version = "v4.0.0-scm"
    }
  with `assert` =
    { dependencies = [ "console", "effect", "prelude" ]
    , repo = "https://github.com/purescm/purescript-assert.git"
    , version = "v6.0.0-scm"
    }
  with console =
    { dependencies = [ "effect", "prelude" ]
    , repo = "https://github.com/purescm/purescript-console.git"
    , version = "v6.0.0-scm"
    }
