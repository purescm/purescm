module Language.PureScript.Scheme.CodeGen.AST where

import Data.Text (Text)

-- | Data type for Scheme expression
data AST

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

  -- | Bound variable reference.
  -- Any identifier appearing as an expression in a program is a variable if a
  -- visible variable binding for the identifier exists.
  -- Check The Scheme Programming Language Fourth Edition Chapter 4.1 for more
  -- information.
  | Identifier Text

  -- | Unquoted list.
  | List [AST]

  deriving (Show)


-- | Recursively apply f to each AST.
everywhere :: (AST -> AST) -> AST -> AST
everywhere f = go where
  go :: AST -> AST
  go (IntegerLiteral i) = f (IntegerLiteral i)
  go (Identifier t) = f (Identifier t)
  go (List xs) = f (List (map go xs))
