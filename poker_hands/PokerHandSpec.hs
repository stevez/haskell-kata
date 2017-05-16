import PokerHands

import Test.Hspec
import Control.Exception (evaluate)

main :: IO()
main = hspec $ do
  describe "High cards" $ do
    it "the highest cards win" $ do
      mkHand ["2C","3C","4C","5C","7C"]  < mkHand ["2D","3D","4D","5D","8D"]

    it "the next highest cards win" $ do
      mkHand ["2C","3C","4C","6C","8C"]  > mkHand ["2D","3D","4D","5D","8D"]

  describe "One pair" $ do
    it "one pair beats high card" $ do
      mkHand ["2C","2H","4C","6C","8D"]  > mkHand ["2D","3D","4D","6D","8C"]

    it "the highest pair win" $ do
      mkHand ["2C","2H","4D","5C","7C"]  < mkHand ["3S","3D","4D","5C","6S"]

    it "highest card win if same pair value" $ do
      mkHand ["2C","2D","4C","3C","7C"]  < mkHand ["2C","2D","4D","6D","8D"]

  --
  describe "Two Pairs"  $ do
    it "two pairs beats one Pair" $ do
      mkHand ["2C","2D","4C","4D","5D"]  > mkHand ["2C","2D","4D","6D","8D"]

    it "the higher values of 2 pairs win" $ do
      mkHand ["3C","3D","5C","5H","7C"]  < mkHand ["2D","2C","6D","6H","4S"]

    it "higher pair same, higher lower pair wins" $ do
      mkHand ["3C","3D","5C","5H","7C"]  < mkHand ["4D","4C","5D","5H","2S"]

    it "same pair, higher remaining card wins" $ do
      mkHand ["3C","3D","5C","5H","7C"]  < mkHand ["3H","3S","5D","5S","8S"]

  describe "Three Of A kind"  $ do
    it "three of a kind beats two Pairs" $ do
      mkHand ["2C","2D","2H","4D","5D"]  > mkHand ["3C","3D","4S","4C","8D"]

    it "higher value of 2 three of a kind wins" $ do
      mkHand ["3C","3D","3H","5H","7C"]  < mkHand ["4D","4C","4S","5H","2S"]

  describe "Straight"  $ do
    it "straight beats three of a kind" $ do
      mkHand ["2C","2D","2H","4D","9D"]  < mkHand ["2S","3D","4S","5C","6D"]
  --
    it "highest card wins for both straight" $ do
      mkHand ["2S","3D","4S","5C","6D"]  < mkHand ["3S","4D","5H","6C","7H"]

  describe "Flush"  $ do
    it "flush beats straight" $ do
      mkHand ["8C","9D","TH","JD","QD"]  < mkHand ["2S","4S","6S","8S","9S"]

    it "high cards win for both flush" $ do
      mkHand ["2S","4S","6S","8S","9S"]  < mkHand ["2C","4C","6C","8C","TC"]

  --
  describe "Full House"  $ do
    it "full house beats flush" $ do
      mkHand ["2S","4S","6S","8S","9S"] < mkHand ["2S","2C","3S","3C","3H"]

   --
    it "high value of 3 cards wins for both fullHouse" $ do
      mkHand ["4S","4H","6S","6C","6D"] >  mkHand ["2S","2C","3S","3C","3H"]


  describe "Four of a kind"  $ do
    it "Four of a kind beats full house" $ do
       mkHand ["2S","2C","8S","8C","8H"] < mkHand ["3S","3C","3D","3H","4H"]

    it "high value of 4 cards wins for both four of a kinds" $ do
      mkHand ["2S","2H","2D","2C","6D"] <  mkHand ["3S","3C","3D","3H","4H"]

  describe "Straight Flush"  $ do
    it "Straing Flush beats Four of a kind" $ do
       mkHand ["TS","TC","TD","TH","AH"] < mkHand ["2S","3S","4S","5S","6S"]
    --
    it "highest card wins for both straight flush cards" $ do
       mkHand ["2S","3S","4S","5S","6S"] < mkHand ["3C","4C","5C","6C","7C"]


  describe "toSuit"  $ do
     it "should convert from code to suit" $ do
        toSuit 'C' `shouldBe` Clubs
        toSuit 'D' `shouldBe` Diamonds
        toSuit 'H' `shouldBe` Hearts
        toSuit 'S' `shouldBe` Spades
        evaluate (toSuit 'A') `shouldThrow` anyException

  describe "play" $ do
    it "should return the winner" $ do
      play "2H 3D 5S 9C KD 2C 3H 4S 8C AH" `shouldBe` "Player 2 wins."
