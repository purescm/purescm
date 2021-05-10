module Language.PureScript.Scheme.CodeGen.Optimizer where

import Language.PureScript.Scheme.CodeGen.AST (AST(..))


runOptimizations :: [AST] -> [AST]
runOptimizations xs = map (\x -> everywhere specializeOperators x) xs


-- TODO: possibly nasty hack. To review once externs are implemented.
specializeOperators :: AST -> AST

specializeOperators (Application (Application (Application (Identifier "Data.Semiring.add")
                                 [Identifier "Data.Semiring.semiringInt"]) [x]) [y])
  = Application (Identifier "+") [x, y]

specializeOperators (Application (Application (Application (Identifier "Data.Ring.sub")
                                 [Identifier "Data.Ring.ringInt"]) [x]) [y])
  = Application (Identifier "-") [x, y]

specializeOperators ast = ast


everywhere :: (AST -> AST) -> AST -> AST
everywhere f = go where
  go :: AST -> AST
  go (VectorLiteral xs) = f (VectorLiteral (map go xs))
  go (Cond xs) = f (Cond (map (\(test, expr) -> (go test, go expr)) xs))
  go (Application function args) = f (Application (go function) (map go args))
  go (Lambda arg expr) = f (Lambda arg (go expr))
  go (Define name expr) = f (Define name (go expr))
  go other = f other
