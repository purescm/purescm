let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.7-20230320/packages.dhall
        sha256:6f2a4b50b793f304d3a64fd25d631de990de280981c73b0683a090e4fa499f0d

in  upstream
  with prelude =
    { dependencies = [] : List Text
    , repo = "https://github.com/purescm/purescript-prelude.git"
    , version = "63ff5a24beba191c8fda919a58489f36bde2d506"
    }
  with partial =
    { dependencies = [] : List Text
    , repo = "https://github.com/purescm/purescript-partial.git"
    , version = "d8b6daf068e4aab0ad85a42d0f890ed37f416923"
    }
