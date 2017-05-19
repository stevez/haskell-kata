module BankOCR where

import Data.Maybe
import Control.Monad
import qualified Data.Map as Map
import Data.List.Split
import Data.List
import Data.Char

type OCR = [String]

one :: [String]
one = [
       "   ",
       "  |",
       "  |"
       ]

two :: [String]
two = [
      " _ ",
      " _|",
      "|_ "
  ]

three :: OCR
three = [
  " _ ",
  " _|",
  " _|"
 ]

four :: OCR
four = [
  "   ",
  "|_|",
  "  |"
 ]

five :: OCR
five = [
  " _ ",
  "|_ ",
  " _|"
 ]

six :: OCR
six = [
   " _ ",
   "|_ ",
   "|_|"
  ]

seven :: OCR
seven = [
   " _ ",
   "  |",
   "  |"
  ]


eight :: OCR
eight = [
   " _ ",
   "|_|",
   "|_|"
  ]

nine :: OCR
nine = [
   " _ ",
   "|_|",
   " _|"
  ]

--
zero :: OCR
zero = [
   " _ ",
   "| |",
   "|_|"
  ]

validOCRs :: [OCR]
validOCRs = [zero,one,two,three, four, five, six, seven, eight, nine]

digitMap :: [([String], Char)]
digitMap = [
            (one, '1'),
            (two, '2'),
            (three, '3'),
            (four, '4'),
            (five, '5'),
            (six,  '6'),
            (seven, '7'),
            (eight, '8'),
            (nine,  '9'),
            (zero,  '0')
           ]


ocrMap :: [(Char,OCR)]
ocrMap = [
           ('1', one),
           ('2', two),
           ('3', three),
           ('4', four),
           ('5', five),
           ('6', six),
           ('7', seven),
           ('8', eight),
           ('9', nine),
           ('0', zero)
         ]


parseDigit :: [String] -> Char
parseDigit xs = fromMaybe '?' (lookup xs digitMap)

parseOCRString :: String -> Char
parseOCRString = parseDigit . chunksOf 3

isValidOCR :: OCR -> Bool
isValidOCR ocr =  map length ocr == [3,3,3]
                  && all (`elem` "|_ ") (concat ocr)
                  && parseDigit ocr `elem` "1234567890?"

showOCR :: OCR -> IO ()
showOCR  = mapM_ putStrLn

showOCRString :: String -> IO ()
showOCRString = showOCR . digitsToOCR

digitToOCR :: Char -> OCR
digitToOCR c = fromMaybe (error "invalid digit") (lookup c ocrMap)

digitToOCRString :: Char -> String
digitToOCRString  = concat . digitToOCR

digitsToOCR :: String -> OCR
digitsToOCR  =  foldr (zipWith (++) . digitToOCR) [[],[],[]]

type Entry = [String]

isEntryValid :: Entry -> Bool
isEntryValid entry =
  all (== True) [ length entry == 4,
                  all ((== 27) . length) entry,
                  all (== ' ') (last entry),
                  all isValidOCR  $ digits entry
                ]

digits :: [String] -> [OCR]
digits xs =  transpose $ map (chunksOf 3) (take 3 xs)

parseEntry :: [String] -> Maybe String
parseEntry entry
  | isEntryValid entry = Just $ map parseDigit $ digits entry
  | otherwise = Nothing

parseEntries :: [String] -> [Maybe String]
parseEntries xs = let entries = chunksOf 4 xs
                  in map parseEntry entries

-- user story 2 --
type Account = String

checksum :: Account -> Int
checksum account = let pairs = zip (map digitToInt account) [9,8..1]
                   in  sum (map (uncurry (*)) pairs) `mod` 11

isCheckSumValid :: Account -> Bool
isCheckSumValid account = checksum account == 0

-- user story 3 --
data Error  =  ERR
             | ILL
             | AMB  deriving (Eq, Show)

data Success = OK deriving (Eq, Show)

validateAcccount :: Account  -> Either Error Success
validateAcccount account
 | isIllegible account = Left ILL
 | checksum account /= 0 = Left ERR
 | otherwise = Right OK


isIllegible :: Account -> Bool
isIllegible = elem '?'


-- story 4 --
options :: Char -> String
options c = optionsOCR (digitToOCR c)

optionsOCR :: OCR -> String
optionsOCR xs = do
      let s = concat xs
      index <- [0..8]
      let (x,h:y) = splitAt index s
      r <- filter (/= h) " |_"
      let modified = x ++ [r] ++ y
      let c' = parseOCRString modified
      guard (c' /= '?')
      return c'


optionsMap :: [(Char,String)]
optionsMap =  map (\c -> (c, options c)) "0123456789"

printOptions :: IO ()
printOptions = mapM_ (\c -> putStrLn ([c] ++ " -> " ++ options c )) "0123456789"

changeOptions :: String -> [String]
changeOptions xs = do
    index <- [0..8]
    let (x, h:y) = splitAt index xs
    r <- options h
    return (x ++ [r] ++ y)

changeOCROptions :: [OCR] -> [String]
changeOCROptions [] = [[]]
changeOCROptions (x:xs) = do
    let c = parseDigit x
    if c == '?'
    then  do
       p <- optionsOCR x
       map (p:) (changeOCROptions xs)
    else map (c:) (changeOCROptions xs)


fixError :: String -> Either Error String
fixError xs = eitherOrFix $ filter isCheckSumValid $ changeOptions xs

fixOCRError :: [OCR] -> Either Error String
fixOCRError xs = eitherOrFix  $ filter isCheckSumValid $ changeOCROptions xs

eitherOrFix :: [String] -> Either Error String
eitherOrFix candidates
  | null candidates        = Left ILL
  | length candidates == 1 = Right (head candidates)
  | otherwise              = Left AMB
