module PureScript.Backend.Chez.Syntax.ChezIdent
  ( EscapedIdent
  , chezIdent
  , unEscape
  ) where

import Prelude

import Data.String as String

newtype EscapedIdent = EscapedIdent String

derive newtype instance Eq EscapedIdent
derive newtype instance Ord EscapedIdent

chezIdent :: String -> EscapedIdent
chezIdent = EscapedIdent <<< String.replaceAll (String.Pattern "'") (String.Replacement "$p")

unEscape :: EscapedIdent -> String
unEscape (EscapedIdent s) = s
