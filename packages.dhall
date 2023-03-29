let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.7-20230320/packages.dhall
        sha256:6f2a4b50b793f304d3a64fd25d631de990de280981c73b0683a090e4fa499f0d

let additions =
      { backend-optimizer =
        { dependencies =
          [ "aff"
          , "ansi"
          , "argonaut"
          , "argonaut-codecs"
          , "argparse-basic"
          , "arrays"
          , "bifunctors"
          , "console"
          , "control"
          , "debug"
          , "dodo-printer"
          , "effect"
          , "either"
          , "enums"
          , "filterable"
          , "foldable-traversable"
          , "foreign-object"
          , "integers"
          , "language-cst-parser"
          , "lazy"
          , "lists"
          , "maybe"
          , "newtype"
          , "node-buffer"
          , "node-child-process"
          , "node-fs"
          , "node-fs-aff"
          , "node-glob-basic"
          , "node-path"
          , "node-process"
          , "node-streams"
          , "ordered-collections"
          , "parallel"
          , "partial"
          , "posix-types"
          , "prelude"
          , "refs"
          , "safe-coerce"
          , "strings"
          , "transformers"
          , "tuples"
          , "unsafe-coerce"
          ]
        , repo =
            "https://github.com/JordanMartinez/purescript-backend-optimizer"
        , version = "add-ty-con-info"
        }
      , node-glob-basic =
        { dependencies =
          [ "aff"
          , "console"
          , "effect"
          , "lists"
          , "maybe"
          , "node-fs-aff"
          , "node-path"
          , "node-process"
          , "ordered-collections"
          , "strings"
          ]
        , repo = "https://github.com/natefaubion/purescript-node-glob-basic.git"
        , version = "v1.2.2"
        }
      }

in  upstream // additions
