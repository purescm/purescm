module Language.PureScript.Scheme.CodeGen.Case where

import Language.PureScript.CoreFn.Ann ( Ann )
import Language.PureScript.CoreFn.Binders ( Binder(..) )
import Language.PureScript.CoreFn.Expr
       ( Expr(..)
       , Guard
       , CaseAlternative(..)
       )

--------------------------------------------------------------------------------

type AExpr = Expr Ann
type ABinder = Binder Ann
type AGuard = Guard Ann
type ACaseAlternative = CaseAlternative Ann

type BoundValue = (AExpr, ABinder)
type GuardedExpr = (AGuard, AExpr)
type Result = Either [GuardedExpr] AExpr

data Alternative = Alternative
  { alternativeBoundValues :: [BoundValue]
  , alternativeResult :: Result
  } deriving ( Show )

--------------------------------------------------------------------------------

-- From:
--   values:
--     [ value_1, value_2, value_3 ]
--   caseAlternatives:
--     [ CaseAlternative
--       { binders = [ binder_a1, binder_a2, binder_a3]
--       , result  = result_a
--       }
--     , CaseAlternative
--       { binders = [ binder_b1, binder_b2, binder_b3 ]
--       , result  = result_b
--       }
--     ]
--
-- To:
--   [ Alternative
--     { alternativeBoundValues = [ ( value_1, binder_a1 )
--                                , ( value_2, binder_a2 )
--                                , ( value_3, binder_a3 )
--                                ]
--     , alternativeResult = result_a
--     }
--   , Alternative
--     { alternativeBoundValues = [ ( value_1, binder_b1 )
--                                , ( value_2, binder_b2 )
--                                , ( value_3, binder_b3 )
--                                ]
--     , alternativeResult = result_b
--     }
--   ]
toAlternatives :: [AExpr] -> [ACaseAlternative] -> [Alternative]
toAlternatives values caseAlternatives
  = map go caseAlternatives
  where
    go :: ACaseAlternative -> Alternative
    go (CaseAlternative binders result)
      = Alternative (zipWith pack values binders) result

    pack :: AExpr -> ABinder -> BoundValue
    pack value binder = (value, binder)
