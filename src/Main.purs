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
{"builtWith":"0.15.7","comments":[],"decls":[{"annotation":{"meta":null,"sourceSpan":{"end":[3,24],"start":[3,1]}},"bindType":"NonRec","expression":{"annotation":{"meta":null,"sourceSpan":{"end":[3,24],"start":[3,17]}},"type":"Literal","value":{"literalType":"StringLiteral","value":"hello"}},"identifier":"literalString"},{"annotation":{"meta":null,"sourceSpan":{"end":[9,15],"start":[9,1]}},"bindType":"NonRec","expression":{"annotation":{"meta":null,"sourceSpan":{"end":[9,15],"start":[9,14]}},"type":"Literal","value":{"literalType":"IntLiteral","value":0}},"identifier":"literalInt"},{"annotation":{"meta":null,"sourceSpan":{"end":[7,19],"start":[7,1]}},"bindType":"NonRec","expression":{"annotation":{"meta":null,"sourceSpan":{"end":[7,19],"start":[7,16]}},"type":"Literal","value":{"literalType":"NumberLiteral","value":0.1}},"identifier":"literalFloat"},{"annotation":{"meta":null,"sourceSpan":{"end":[5,18],"start":[5,1]}},"bindType":"NonRec","expression":{"annotation":{"meta":null,"sourceSpan":{"end":[5,18],"start":[5,15]}},"type":"Literal","value":{"literalType":"CharLiteral","value":"a"}},"identifier":"literalChar"},{"annotation":{"meta":null,"sourceSpan":{"end":[11,23],"start":[11,1]}},"bindType":"NonRec","expression":{"annotation":{"meta":null,"sourceSpan":{"end":[11,23],"start":[11,18]}},"type":"Literal","value":{"literalType":"BooleanLiteral","value":false}},"identifier":"literalBoolean"}],"exports":["literalString","literalChar","literalFloat","literalInt","literalBoolean"],"foreign":[],"imports":[{"annotation":{"meta":null,"sourceSpan":{"end":[11,23],"start":[1,1]}},"moduleName":["Prim"]}],"moduleName":["Simple"],"modulePath":"src/Simple.purs","reExports":{},"sourceSpan":{"end":[11,23],"start":[1,1]}}
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
