module PascalsTriangle.Test where
import PascalsTriangle (pascalsTriangle)
import Test.Hspec
import Test.QuickCheck

main = hspec $ do
  describe "pascalsTriangle" $ do
    it "should work for some examples" $ do
      pascalsTriangle 1  `shouldBe` [1]
      pascalsTriangle 2  `shouldBe` [1,1,1]
      pascalsTriangle 3  `shouldBe` [1,1,1,1,2,1]
