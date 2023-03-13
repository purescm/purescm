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
{"builtWith":"0.15.8","comments":[],"decls":[{"annotation":{"meta":null,"sourceSpan":{"end":[9,12],"start":[5,1]}},"bindType":"NonRec","expression":{"annotation":{"meta":null,"sourceSpan":{"end":[9,12],"start":[6,3]}},"binds":[{"annotation":{"meta":null,"sourceSpan":{"end":[7,18],"start":[7,5]}},"bindType":"NonRec","expression":{"abstraction":{"abstraction":{"annotation":{"meta":{"metaType":"IsForeign"},"sourceSpan":{"end":[7,12],"start":[7,9]}},"type":"Var","value":{"identifier":"add","moduleName":["Simple"]}},"annotation":{"meta":null,"sourceSpan":{"end":[7,15],"start":[7,9]}},"argument":{"annotation":{"meta":null,"sourceSpan":{"end":[7,15],"start":[7,13]}},"type":"Literal","value":{"literalType":"IntLiteral","value":42}},"type":"App"},"annotation":{"meta":null,"sourceSpan":{"end":[7,18],"start":[7,9]}},"argument":{"annotation":{"meta":null,"sourceSpan":{"end":[7,18],"start":[7,16]}},"type":"Literal","value":{"literalType":"IntLiteral","value":42}},"type":"App"},"identifier":"a"}],"expression":{"abstraction":{"abstraction":{"annotation":{"meta":{"metaType":"IsForeign"},"sourceSpan":{"end":[9,8],"start":[9,5]}},"type":"Var","value":{"identifier":"add","moduleName":["Simple"]}},"annotation":{"meta":null,"sourceSpan":{"end":[9,10],"start":[9,5]}},"argument":{"annotation":{"meta":null,"sourceSpan":{"end":[9,10],"start":[9,9]}},"type":"Var","value":{"identifier":"a","sourcePos":[7,5]}},"type":"App"},"annotation":{"meta":null,"sourceSpan":{"end":[9,12],"start":[9,5]}},"argument":{"annotation":{"meta":null,"sourceSpan":{"end":[9,12],"start":[9,11]}},"type":"Var","value":{"identifier":"a","sourcePos":[7,5]}},"type":"App"},"type":"Let"},"identifier":"notBasic"}],"exports":["add","notBasic"],"foreign":["add"],"imports":[{"annotation":{"meta":null,"sourceSpan":{"end":[9,12],"start":[1,1]}},"moduleName":["Prim"]},{"annotation":{"meta":null,"sourceSpan":{"end":[9,12],"start":[1,1]}},"moduleName":["Simple"]}],"moduleName":["Simple"],"modulePath":"src/Simple.purs","reExports":{},"sourceSpan":{"end":[9,12],"start":[1,1]}}
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
