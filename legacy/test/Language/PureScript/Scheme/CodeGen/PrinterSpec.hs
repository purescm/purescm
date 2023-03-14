module Language.PureScript.Scheme.CodeGen.PrinterSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Language.PureScript.Scheme.CodeGen.SExpr (SExpr(..))
import Language.PureScript.Scheme.CodeGen.Printer (printSExpr)

spec :: Spec
spec = do
  describe "String printing" $ stringSpec

stringSpec :: Spec
stringSpec = do
  it "can print simple string" $ do
    printSExpr (String "foo") `shouldBe` "\"foo\""
  it "can print quoted string" $ do
    printSExpr (String "\"foo\"") `shouldBe` "\"\\\"foo\\\"\""
