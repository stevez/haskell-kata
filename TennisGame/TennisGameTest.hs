import TennisGame

import Test.Hspec
import Test.QuickCheck
import Text.Printf (printf)

testScoreGame :: (Int, Int, String) -> Spec
testScoreGame (p1, p2, expected) =
      it( printf "%d : %d -> %s" p1 p2 expected ) $
        gameScore p1 p2 `shouldBe` expected

testData :: [(Int,Int,String)]
testData = [
     (0, 0, "Love-All"),
     (1, 1, "Fifteen-All"),
     (2, 2, "Thirty-All"),
     (3, 3, "Deuce"),
     (4, 4, "Deuce"),

     (0, 1, "Love-Fifteen"),
     (1, 0, "Fifteen-Love"),
     (0, 2, "Love-Thirty"),
     (2, 0, "Thirty-Love"),
     (0, 3, "Love-Forty"),
     (3, 0, "Forty-Love"),

     (0, 4, "Win for player2"),
     (4, 0, "Win for player1"),
     (4, 1, "Win for player1"),
     (1, 4, "Win for player2"),
     (4, 2, "Win for player1"),
     (2, 4, "Win for player2"),
     (4, 3, "Advantage player1"),
     (3, 4, "Advantage player2"),
     (5, 4, "Advantage player1"),
     (4, 5, "Advantage player2"),
     (15, 14, "Advantage player1"),
     (14, 15, "Advantage player2"),
     (6, 4, "Win for player1"),
     (4, 6, "Win for player2"),
     (16, 14, "Win for player1"),
     (14, 16, "Win for player2")  
  ]

main :: IO ()
main = hspec $ do
  describe "scoreGame" $ do
     mapM_ testScoreGame testData
