module Main where

import Prelude

import Control.Alternative (empty)
import Data.Argonaut as Json
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Dodo (plainText)
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (error, log)
import PureScript.Backend.Chez.Convert (codegenModule)
import PureScript.Backend.Chez.Syntax as S
import PureScript.Backend.Optimizer.Builder (buildModules)
import PureScript.Backend.Optimizer.CoreFn (Ann, Module)
import PureScript.Backend.Optimizer.CoreFn.Json (decodeModule)

simpleCoreFn :: String
simpleCoreFn =
  """
{"builtWith":"0.15.7","comments":[],"decls":[{"annotation":{"meta":null,"sourceSpan":{"end":[3,31],"start":[3,1]}},"bindType":"NonRec","expression":{"annotation":{"meta":null,"sourceSpan":{"end":[3,31],"start":[3,17]}},"type":"Literal","value":{"literalType":"ObjectLiteral","value":[["a",{"annotation":{"meta":null,"sourceSpan":{"end":[3,23],"start":[3,22]}},"type":"Literal","value":{"literalType":"IntLiteral","value":0}}],["b",{"annotation":{"meta":null,"sourceSpan":{"end":[3,29],"start":[3,28]}},"type":"Literal","value":{"literalType":"IntLiteral","value":1}}]]}},"identifier":"literalRecord"},{"annotation":{"meta":null,"sourceSpan":{"end":[5,27],"start":[5,1]}},"bindType":"NonRec","expression":{"annotation":{"meta":null,"sourceSpan":{"end":[5,27],"start":[5,16]}},"type":"Literal","value":{"literalType":"ArrayLiteral","value":[{"annotation":{"meta":null,"sourceSpan":{"end":[5,19],"start":[5,18]}},"type":"Literal","value":{"literalType":"IntLiteral","value":1}},{"annotation":{"meta":null,"sourceSpan":{"end":[5,22],"start":[5,21]}},"type":"Literal","value":{"literalType":"IntLiteral","value":2}},{"annotation":{"meta":null,"sourceSpan":{"end":[5,25],"start":[5,24]}},"type":"Literal","value":{"literalType":"IntLiteral","value":3}}]}},"identifier":"literalArray"}],"exports":["literalRecord","literalArray"],"foreign":[],"imports":[{"annotation":{"meta":null,"sourceSpan":{"end":[5,27],"start":[1,1]}},"moduleName":["Prim"]}],"moduleName":["Simple"],"modulePath":"src/Simple.purs","reExports":{},"sourceSpan":{"end":[5,27],"start":[1,1]}}
"""

parseCoreFnModule :: String -> Aff (Either String (Module Ann))
parseCoreFnModule contents = pure $ lmap Json.printJsonDecodeError <<< decodeModule =<<
  Json.jsonParser contents

main :: Effect Unit
main = launchAff_ do
  coreFnModule <- parseCoreFnModule simpleCoreFn
  case coreFnModule of
    Left e -> error e
    Right m -> do
      pure m # buildModules
        { directives: empty
        , foreignSemantics: empty
        , onCodegenModule: \_ _ backendModule -> do
            log $ Dodo.print plainText Dodo.twoSpaces $ S.printChezExpr $ codegenModule
              backendModule
            pure unit
        , onPrepareModule: \_ coreFnM -> pure coreFnM
        }
