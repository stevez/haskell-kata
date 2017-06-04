import Test.Hspec
import Permutations

main = hspec $ do
  describe "permutations" $ do
    it "should work for some examples" $ do
      permutations    "a" `shouldBe` ["a"]
      permutations   "ab" `shouldBe` ["ab", "ba"]
      permutations "aabb" `shouldBe` ["aabb","abab","abba","baab","baba","bbaa"]
