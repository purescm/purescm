{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.PureScript.Scheme.CodeGen.Printer
  ( printLibrary
  , printSExpr
  ) where

import Data.Text (Text)
import Language.PureScript.PSString (prettyPrintStringJS)
import Language.PureScript.Scheme.CodeGen.SExpr (SExpr(..))
import Language.PureScript.Scheme.CodeGen.Library (Library(..))
import Prettyprinter (Pretty(..))

import qualified Data.Text as Text
import qualified Prettyprinter as Pretty

list :: forall a. [Pretty.Doc a] -> Pretty.Doc a
list [] = tpretty "()"
list [x] = Pretty.enclose Pretty.lparen Pretty.rparen x
list (x:xs) = Pretty.nest 2
            $ Pretty.sep ([Pretty.lparen <> x] <> xs) <> Pretty.rparen 

tpretty :: Text -> Pretty.Doc a
tpretty = pretty

instance Pretty SExpr where
  pretty (Integer x) = pretty x
  pretty (Float x) = pretty x
  pretty (String x) = pretty (prettyPrintStringJS x)
  pretty (Character x) = pretty ("#\\" <> Text.pack [x])
  pretty (Boolean x) = tpretty (if x then "#t" else "#f")
  pretty (Symbol x) = pretty x
  pretty (List xs) = list (map pretty xs)

instance Pretty Library where
  pretty Library{..} = list $
    [ tpretty "library"
    , list [ name, tpretty "lib" ]
    , list (tpretty "export" : exports)
    , list $
      [ tpretty "import"
      , rnrsImport
      ] <> foreignImports <> otherImports
    , Pretty.hardline
    ] <> fmap (\e -> pretty e <> Pretty.hardline) libraryBody

    where
      name = pretty libraryName
      exports = map pretty libraryExports

      rnrsImport = list [ tpretty "prefix"
                        , list [ tpretty "rnrs"]
                        , tpretty "scm:"
                        ]

      foreignImports = case libraryForeigns of
        [] -> []
        fs ->
          [ list $
              [ tpretty "only"
              , list [ name, tpretty "foreign" ]
              ] <> map pretty fs
          ]

      otherImports = map
        (\n -> list [ tpretty "prefix"
                    , list [ pretty n, tpretty "lib" ]
                    , pretty n <> tpretty "."])
        libraryImports

printLibrary :: Library -> Text
printLibrary = Text.pack . show . Pretty.pretty

printSExpr :: SExpr -> Text
printSExpr = Text.pack . show . Pretty.pretty
