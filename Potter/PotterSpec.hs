import Potter
import Test.Hspec
import Control.Exception (evaluate)

main :: IO()
main = hspec $ do
  describe "Basics" $ do
    it "should be zero for empty array" $ do
      price []  `shouldBe` 0

    it "should be no discount for buy 1 book" $ do
       price [0] `shouldBe` 8
       price [1] `shouldBe` 8
       price [2] `shouldBe` 8
       price [3] `shouldBe` 8
       price [4] `shouldBe` 8

    it "should be no discount if buy multiple same books" $ do
        price [0,0] `shouldBe` 8 * 2
        price [1,1,1] `shouldBe` 8 * 3

  describe "Simple discount" $ do
    it "should be 5% discount for 2 different books" $ do
      price [0,1] `shouldBe` 8 * 2 * 0.95

    it "should be 10% discount for 3 different books" $ do
      price [0,1,2] `shouldBe` 8 * 3 * 0.9

    it "should be 20% off for 4 different books" $ do
      price [0,1,2,4] `shouldBe` 8 * 4 * 0.8

    it "should be 25% off for 5 different books" $ do
      price [0,1,2,4,5] `shouldBe` 8 * 5 * 0.75

  describe "several discunts" $ do
    it "should be applied to multiple discounts" $ do
      price [0,0,1] `shouldBe` 8 + (8 * 2 * 0.95)
      price [0,0,1,1] `shouldBe` 2 * (8 * 2 * 0.95)
      price [0, 0, 1, 2, 2, 3] `shouldBe` (8 * 4 * 0.8) + (8 * 2 * 0.95)
      price [0, 1, 1, 2, 3, 4] `shouldBe` 8 + (8 * 5 * 0.75)

  describe  "Edge cases" $ do
     it "should find the minimum amount" $ do
       price [0, 0, 1, 1, 2, 2, 3, 4] `shouldBe` 2 * (8 * 4 * 0.8)
