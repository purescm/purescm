module Language.PureScript.Scheme.MakeSpec (spec) where

import Data.Text (Text)
import Test.Hspec (Spec, it, shouldBe, runIO, beforeAll_)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Language.PureScript.Scheme.TestUtil as TestUtil

testModule :: Text -> Spec
testModule fixturePath = do
  it ("Can compile " <> Text.unpack fixturePath) $ do
    compilationResult
      <- Text.IO.readFile
       $ Text.unpack
       $ TestUtil.schemeOutput fixturePath
    expectedResult <- Text.IO.readFile (Text.unpack fixturePath)
    compilationResult `shouldBe` expectedResult

spec :: Spec
spec = do
  fixturePaths <- runIO TestUtil.findSchemeFixtures
  beforeAll_ TestUtil.spagoBuild $ mapM_ testModule fixturePaths
