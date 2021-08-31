module Language.PureScript.Scheme.CodeGen.Library where

import Data.Text ( Text )
import Language.PureScript.Scheme.CodeGen.SExpr ( SExpr )

data Library = Library
  { libraryName :: Text
  , libraryExports :: [Text]
  , libraryImports :: [Text]
  , libraryBody :: [SExpr]
  }
