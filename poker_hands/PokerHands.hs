module PokerHands where

import Data.List
import Data.List.Split
import Data.Function
import Data.Maybe
import Control.Monad

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Show, Eq, Ord)

suitMap :: [(Char,Suit)]
suitMap = [('C', Clubs),
           ('D', Diamonds),
           ('H', Hearts),
           ('S', Spades)]

toSuit :: Char -> Suit
toSuit c = fromMaybe (error "invalid suit code") (lookup c suitMap)

data Rank =  Two
            | Three
            | Four
            | Five
            | Six
            | Seven
            | Eight
            | Nine
            | Ten
            | Jack
            | Queen
            | King
            | Ace
            deriving (Show, Eq, Ord, Enum)

rankMap :: [(Char,Rank)]
rankMap = [ ('2', Two),
            ('3', Three),
            ('4', Four),
            ('5', Five),
            ('6', Six),
            ('7', Seven),
            ('8', Eight),
            ('9', Nine),
            ('T', Ten),
            ('J', Jack),
            ('Q', Queen),
            ('K', King),
            ('A', Ace)]

toRank :: Char -> Rank
toRank c = fromMaybe (error "invalid rank code") (lookup c rankMap)

data Card = Card Rank Suit deriving (Show)

instance Eq Card where
  (==) (Card r1 _) (Card r2 _) = (==) r1 r2

instance Ord Card where
  compare = compare `on` rank

data Hand = Hand [Card] deriving (Show, Eq)

instance Ord Hand where
  compare = compare `on` handValue


type Kicker = Rank

data HandValue =  HighCard [Kicker]
                | OnePair Rank [Kicker]
                | TwoPairs [Rank] [Kicker]
                | ThreeOfAKind Rank [Kicker]
                | Straight Rank
                | Flush [Rank]
                | FullHouse Rank
                | FourOfAKind Rank
                | StraightFlush Rank
                deriving (Show, Eq, Ord)


rank :: Card -> Rank
rank (Card r s) = r

ranks :: Hand -> [Rank]
ranks = reverse . sort . map rank . cards

suit :: Card -> Suit
suit (Card r s) = s

suits :: Hand -> [Suit]
suits = map suit . cards

cards :: Hand -> [Card]
cards (Hand cs) = cs


handValue :: Hand -> HandValue
handValue h = fromJust $ foldl (\acc f -> mplus acc (f h) ) Nothing hands
              where hands = [
                             straightFlush,
                             fourOfAKind,
                             fullHouse,
                             flush,
                             straight,
                             threeOfAKind,
                             twoPairs,
                             onePair,
                             highCard
                            ]

highCard :: Hand -> Maybe HandValue
highCard  = Just . HighCard . ranks

onePair :: Hand -> Maybe HandValue
onePair h =  let rs = ranks h
                 pairs = nOfAKind 2 rs
             in case pairs of
                [x]  -> Just $ OnePair x [y | y <- rs, y `notElem`[x]]
                _        -> Nothing

twoPairs :: Hand -> Maybe HandValue
twoPairs h =  let rs = ranks h
                  pairs = nOfAKind 2 rs
              in case pairs of
                [x,y]  -> Just $ TwoPairs [x,y] [z | z <- rs, z `notElem` [x,y]]
                _        -> Nothing

threeOfAKind :: Hand -> Maybe HandValue
threeOfAKind h =  let rs = ranks h
                      kinds = nOfAKind 3 rs
                  in case kinds of
                       [x]  -> Just $ ThreeOfAKind x [z | z <- rs, z `notElem` [x]]
                       _        -> Nothing

fourOfAKind :: Hand -> Maybe HandValue
fourOfAKind h = if isFourOfAKind h
                then Just $ FourOfAKind (head $ nOfAKind 4 $ ranks h)
                else Nothing

nOfAKind :: Int -> [Rank] -> [Rank]
nOfAKind n h =  map head . filter ((==n) . length) $ group  h

straight :: Hand -> Maybe HandValue
straight h = if isStraight h
             then Just $ Straight (head (ranks h))
             else Nothing

isStraight :: Hand -> Bool
isStraight h =  all (\(a,b) -> a == succ b) $ zip (init rs) (tail rs)
                where rs = ranks h


flush :: Hand -> Maybe HandValue
flush h = if isFlush h
          then Just $ Flush (ranks h)
          else Nothing

isFlush :: Hand -> Bool
isFlush h = let s = suits h
                in all (== head s) s

isStraightFlush :: Hand -> Bool
isStraightFlush h = isStraight h && isFlush h


isPair :: Hand -> Bool
isPair =  (==1) . length . nOfAKind 2 . ranks

isThreeOfAKind :: Hand -> Bool
isThreeOfAKind =  (==1) . length . nOfAKind 3 . ranks

isFourOfAKind :: Hand -> Bool
isFourOfAKind =  (==1) . length . nOfAKind 4 . ranks

fullHouse :: Hand -> Maybe HandValue
fullHouse h = if isPair h && isThreeOfAKind h
              then Just $ FullHouse (head $ nOfAKind 3 $ ranks h)
              else Nothing

straightFlush :: Hand -> Maybe HandValue
straightFlush h = if isStraightFlush h
                  then Just $ StraightFlush (head (ranks h))
                  else Nothing


mkCard :: String -> Card
mkCard [rank,suit] = Card (toRank rank) (toSuit suit)

mkHand :: [String] -> Hand
mkHand = Hand . reverse . sort . map mkCard

play :: String -> String
play string =  do
   let cs = filter (not . null) $ splitOn " " string
   let (p1,p2)  = splitAt 5 cs
   case compare (mkHand p1) (mkHand p2) of
        LT  -> "Player 2 wins."
        GT  -> "Player 1 wins."
        EQ  -> "Tie."
