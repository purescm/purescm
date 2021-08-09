module Language.PureScript.Scheme.CodeGen.PrinterSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Language.PureScript.Scheme.CodeGen.SExpr (SExpr(..))
import Language.PureScript.Scheme.CodeGen.Printer (emit)

spec :: Spec
spec = do
  describe "String printing" $ stringSpec

stringSpec :: Spec
stringSpec = do
  it "can print simple string" $ do
    emit (String "foo") `shouldBe` "\"foo\""
  it "can print quoted string" $ do
    emit (String "\"foo\"") `shouldBe` "\"\\\"foo\\\"\""
