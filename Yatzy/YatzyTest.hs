module YatzyTest where

import Yatzy

import Test.Hspec

main :: IO()
main = hspec $ do
 describe "Chance" $ do
  it "scores sum of all dice" $ do
    chance [2, 3, 4, 5, 1] `shouldBe` 15
    chance [3, 3, 4, 5, 1] `shouldBe` 16

 describe "Yatzy" $ do
   it "scores 50" $ do
     yatzy [4,4,4,4,4] `shouldBe` 50
     yatzy [6,6,6,6,6] `shouldBe` 50
     yatzy [6,6,6,6,3] `shouldBe` 0

 describe "Ones" $ do
   it "scores the sum of 1s" $ do
     ones [1,2,3,4,5] `shouldBe` 1
     ones [1,2,1,4,5] `shouldBe` 2
     ones [6,2,2,4,5] `shouldBe` 0
     ones [1,2,1,1,1] `shouldBe` 4

 describe "Twos" $ do
   it "score the sum of 2s" $ do
         twos [1,2,3,2,6] `shouldBe` 4
         twos [2,2,2,2,2] `shouldBe` 10

 describe "Threes" $ do
    it "score the sum of 3s" $ do
        threes [1,2,3,2,3] `shouldBe` 6
        threes [2,3,3,3,3] `shouldBe` 12

 describe "Fours" $ do
   it "scores the sum of 4s" $ do
     fours [4,4,4,5,5] `shouldBe` 12
     fours [4,4,5,5,5] `shouldBe` 8
     fours [4,5,5,5,5] `shouldBe` 4

 describe "Fives" $ do
   it "scores the sum of 5s" $ do
     fives [4,4,4,5,5] `shouldBe` 10
     fives [4,4,5,5,5] `shouldBe` 15
     fives [4,5,5,5,5] `shouldBe` 20

 describe "Sixes" $ do
   it "scores the sum of 6s" $ do
     sixes [4,4,4,5,5] `shouldBe` 0
     sixes [4,4,6,5,5] `shouldBe` 6
     sixes [6,5,6,6,5] `shouldBe` 18

 describe "One pair" $ do
   it "scores the sum of the highest pair" $ do
     score_pair [3,4,3,5,6] `shouldBe` 6
     score_pair [5,3,3,3,5] `shouldBe` 10
     score_pair [5,3,6,6,5] `shouldBe` 12
     score_pair [1,2,3,4,5] `shouldBe` 0

 describe "Two pair" $ do
   it "scores the sum of the two pairs" $ do
     two_pair [3,3,5,4,5] `shouldBe` 16
     two_pair [3,3,5,5,5] `shouldBe` 16
     two_pair [1,2,3,4,5] `shouldBe` 0
     two_pair [1,3,6,6,5] `shouldBe` 0


 describe "Three of a kind" $ do
  it "scores the sum of the three of the kind" $ do
    three_of_a_kind [3,3,3,4,5] `shouldBe` 9
    three_of_a_kind [5,3,5,4,5] `shouldBe` 15
    three_of_a_kind [3,3,3,3,5] `shouldBe` 9

 describe "Four of a kind" $ do
    it "scores the sum of the four of the kind" $ do
        four_of_a_kind [3,3,3,3,5] `shouldBe` 12
        four_of_a_kind [5,5,5,4,5] `shouldBe` 20

 describe "Small straight" $ do
    it "scores 15" $ do
       smallStraight [1,2,3,4,5] `shouldBe` 15
       smallStraight [2,3,4,5,1] `shouldBe` 15
       smallStraight [1,2,2,4,5] `shouldBe` 0

 describe "Large straight" $ do
   it "scores 20" $ do
      largeStraight [6,2,3,4,5] `shouldBe` 20

 describe "Full house" $ do
    it "scores the sum of the full house" $ do
        fullHouse [6,2,2,2,6] `shouldBe` 18
        fullHouse [2,3,4,5,6] `shouldBe` 0
