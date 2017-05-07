module TennisGame where

data Score = Love | Fifteen | Thirty | Forty | More  deriving (Eq, Ord, Enum, Show)

score :: Int -> Score
score 0 = Love
score 1 = Fifteen
score 2 = Thirty
score 3 = Forty
score n = More



gameScore :: Int -> Int -> String
gameScore p1 p2
  | p2 >= 4 && (p2 - p1) >= 2 = "Win for player2"
  | p1 >= 4 && (p1 - p2) >= 2 = "Win for player1"
  | all (>=3) [p1,p2] && (p2 - p1 == 1) = "Advantage player2"
  | all (>=3) [p1,p2] && (p1 - p2 == 1) = "Advantage player1"
  | p1 == p2 && p1 >= 3  = "Deuce"
  | s1 == s2                 = show s1 ++ "-All"
  | otherwise                = show s1 ++ "-" ++ show s2
  where s1 = score p1
        s2 = score p2
