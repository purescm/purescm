module Language.PureScript.Scheme.CodeGen.SExpr where

import Data.Text (Text)

-- | Data type for symbolic expressions.
data SExpr

  -- | Integer literal. Can be either a fixnum or a bignum.
  --   - Fixnums represent integers in the fixnum range accessible through
  --     the most-negative-fixnum and most-positive-fixnum procedures.
  --     The length of a string, vector, or fxvector is constrained to be a
  --     fixnum.
  --   - Bignums represent arbitrary-precision integers outside of the fixnum
  --     range.
  = Integer Integer

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
  go (Integer i) = f (Integer i)
  go (Symbol t) = f (Symbol t)
  go (List xs) = f (List (map go xs))
