module Language.PureScript.Scheme.MakeSpec (spec) where

import qualified Data.Text                       as Text
import           Data.Text                       (Text)
import qualified Data.Text.IO                    as Text.IO
import qualified Turtle                          as Turtle
import           Test.Hspec                      (Spec, it, shouldBe,
                                                  runIO, beforeAll_)
import           TestUtil                        (corefnFile, schemeFile,
                                                  findModules, buildCorefn)
import           Language.PureScript.Scheme.Make (compile)

testModule :: Text -> Spec
testModule module_ = do
  it ("Can compile " <> Text.unpack module_) $ do
    compilationResult <- compile $ Turtle.encodeString $ corefnFile module_
    expectedResult <- Text.IO.readFile $ Turtle.encodeString $ schemeFile module_
    compilationResult `shouldBe` expectedResult
  
spec :: Spec
spec = do
  ms <- runIO findModules
  beforeAll_ (buildCorefn . removeCorefnFiles) $ mapM_ testModule ms
