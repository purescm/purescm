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

  -- | Vector literal.
  -- Whereas accessing an arbitrary element in a list requires a linear traversal
  -- of the list up to the selected element, arbitrary vector elements are
  -- accessed in constant time.
  -- Vectors can't be resized.
  -- Check The Scheme Programming Language Fourth Edition Chapter 6.9 for more
  -- information.
  | VectorLiteral [AST]

  -- | Variable definition.
  -- In Scheme there are two define syntax:
  --   - Simple variable definition: (define var expr). This expression binds
  --     var to expr.
  --   - Unspecified definition: (define var)
  --   - Procedure definition:
  --       - (define (var0 var1 ...) body1 body2 ...)
  --       - (define (var0 . vars) body1 body2 ...)
  --       - (define (var0 var1 ... . vars) body1 body2)
  -- This data construtor expresses a simple variable definition.
  -- Check The Scheme Programming Language Fourth Edition Chapter 4.6 for more
  -- information.
  | Define Text AST

  deriving (Show)
