let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.7-20230306/packages.dhall
        sha256:0757626c7422b8b5b5b1d0df3d3628e5deac755d7f89c433a9bf89009787dcbd

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
            "https://github.com/aristanetworks/purescript-backend-optimizer"
        , version = "purs-backend-es-v1.3.2"
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
