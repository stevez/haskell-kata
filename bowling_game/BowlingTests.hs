module BowlingTests where

import BowlingGame

import Test.Hspec
import Test.QuickCheck
import Text.Printf (printf)

testScoreGame :: String -> Int -> Spec
testScoreGame game expected =
      it( printf "should return the score for game : %s --> %d \n" game expected ) $
        score game `shouldBe` expected

main = hspec $ do
    describe "scoreGame" $ do
        testScoreGame"--------------------" 0
        testScoreGame "1-1----------------1" 3
        testScoreGame "9-9-9-9-9-9-9-9-9-9-" 90
        testScoreGame "1-1-1-1-1-1-1-1-1-1-" 10
        testScoreGame "12131415161718171611" 59
        testScoreGame "5/5/5/5/5/5/5/5/5/5/5" 150
        testScoreGame "XXXXXXXXXXXX" 300
        testScoreGame "XXXXXXXXXX12" 274
        testScoreGame "1/35XXX458/X3/23" 160
        testScoreGame "1/35XXX458/X3/XX6" 189
        testScoreGame "5/11------------3/11" 26
