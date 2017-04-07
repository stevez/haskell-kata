import PrimeFactors

import Test.Hspec

main :: IO()
main = hspec $ do
  describe "Prime Factors" $ do
    it "should 1 -> []" $ do
      primeFactors 1 `shouldBe` []

    it "should 2 -> [2]" $ do
      primeFactors 2 `shouldBe` [2]

    it "should 3 -> [3]" $ do
      primeFactors 3 `shouldBe` [3]

    it "should 4 -> [2,2]" $ do
      primeFactors 4 `shouldBe` [2,2]

    it "should 6 -> [2,3]" $ do
      primeFactors 6 `shouldBe` [2,3]

    it "should 8 -> [2,2,2]" $ do
      primeFactors 8 `shouldBe` [2,2,2]

    it "should 9 -> [3,3]" $ do
      primeFactors 9 `shouldBe` [3,3]

    it "should 15 -> [3,5]" $ do
      primeFactors 15 `shouldBe` [3,5]

    it "should 315 -> [3,3,5,7]" $ do
      primeFactors 315 `shouldBe` [3,3,5,7]
