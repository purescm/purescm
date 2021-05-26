module Language.PureScript.Scheme.MakeSpec (spec) where

import qualified Data.Text.IO                    as T.IO
import           Test.Hspec                      (Spec, it, shouldBe)
import           Language.PureScript.Scheme.Make (compile)

basePath :: String
basePath = "test/resources"
  
modules :: [String]
modules = [ "PureScmTest.Literals"
          , "PureScmTest.Fib"
          , "PureScmTest.FunctionMultipleMatch"
          , "PureScmTest.MutuallyRecursiveFunction"
          , "PureScmTest.GuardedFunction"
          , "PureScmTest.Constructor"
          , "PureScmTest.NestedConstructor"
          , "PureScmTest.NamedBinder"
          ]

coreFnPath :: String -> FilePath
coreFnPath module_ = basePath <> "/" <> module_ <> ".corefn.json"

schemePath :: String -> FilePath
schemePath module_ = basePath <> "/" <> module_ <> ".sls"

testModule :: String -> Spec
testModule module_ = do
  it ("Can compile " <> module_) $ do
    compilationResult <- compile $ coreFnPath module_
    expectedResult <- T.IO.readFile $ schemePath module_
    compilationResult `shouldBe` expectedResult
  
spec :: Spec
spec = mapM_ testModule modules
