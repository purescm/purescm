module Language.PureScript.Scheme.CodeGen.SExpr where

import Data.Text (Text)

-- | Data type for Scheme expression
data SExpr

  -- | Integer literal.
  -- Chez Scheme uses two distinct types for integers:
  --   - Fixnums represent integers in the fixnum range accessible through
  --     the most-negative-fixnum and most-positive-fixnum procedures.
  --     The length of a string, vector, or fxvector is constrained to be a
  --     fixnum.
  --   - Bignums represent arbitrary-precision integers outside of the fixnum
  --     range.
  -- Chez Scheme exposes fast, type-specific numeric operations on fixnums
  -- but they are not used by PureScheme (yet).
  -- Check Chez Scheme Version 9 User's Guide chapter 8 for more information.
  = IntegerLiteral Integer

  -- | An unquoted symbol.
  -- E.g. in `(lambda (x) (+ x 1))': `lambda', `x' and `+' are symbols.
  | Symbol Text

  -- | Unquoted list.
  | List [SExpr]

  deriving (Show)


-- | Recursively apply f to each SExpr.
everywhere :: (SExpr -> SExpr) -> SExpr -> SExpr
everywhere f = go where
  go :: SExpr -> SExpr
  go (IntegerLiteral i) = f (IntegerLiteral i)
  go (Symbol t) = f (Symbol t)
  go (List xs) = f (List (map go xs))
