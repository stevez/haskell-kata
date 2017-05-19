import Test.Hspec
import Control.Exception (evaluate)

import BankOCR

entry1 =
  ["    _  _     _  _  _  _  _ ",
   "  | _| _||_||_ |_   ||_||_|",
   "  ||_  _|  | _||_|  ||_| _|",
   "                           "]
entry_3_lines =
  ["    _  _     _  _  _  _  _ ",
   "  | _| _||_||_ |_   ||_||_|",
   "  ||_  _|  | _||_|  ||_| _|"]

--
entry_28_chars =
  ["    _  _     _  _  _  _  _  ",
   "  | _| _||_||_ |_   ||_||_|",
   "  ||_  _|  | _||_|  ||_| _|",
   "                           "]

--
entryLastLineNonSpace =
  ["    _  _     _  _  _  _  _ ",
   "  | _| _||_||_ |_   ||_||_|",
   "  ||_  _|  | _||_|  ||_| _|",
   "_                          "]

--
entryHasEqualCharacter =
    ["  = _  _     _  _  _  _  _ ",
     "  | _| _||_||_ |_   ||_||_|",
     "  ||_  _|  | _||_|  ||_| _|",
     "                           "]

--
entryIllegibleDigit =
  ["    _  _     _  _  _  _  _ ",
   "  | _| _||_||_ |_   ||_||_|",
   "  ||_| _|  | _||_|  ||_| _|",
   "                           "]

--
entry2IllegibleDigits =
  ["    _  _     _  _  _  _  _ ",
   "  | _| _||_||_ |_   ||_| _|",
   "  ||_| _|  | _||_|  ||_||_|",
   "                           "]

--
entryIllegibleNoOption =
  ["    _  _     _  _  _  _  _ ",
   "    _| _||_||_ |_   ||_||_|",
   "   |_| _|  | _||_|  ||_| _|",
   "                           "]

--
twoEntries =
  ["    _  _     _  _  _  _  _ ",
   "  | _| _||_||_ |_   ||_||_|",
   "  ||_  _|  | _||_|  ||_| _|",
   "                           ",
   "    _  _     _  _  _  _  _ ",
   "  | _| _||_||_ |_   ||_||_|",
   "  ||_  _|  | _||_|  ||_| _|",
   "                           "
   ]

--
twoEntriesWithInvalid =
  ["    _  _     _  _  _  _  _ ",
   "  | _| _||_||_ |_   ||_||_|",
   "  ||_  _|  | _||_|  ||_| _|",
   "                           ",
   "    _  _     _  _  _  _  _ ",
   "  | _| _||_||_ |_   ||_||_|",
   "  ||_| _|  | _||_|  ||_| _|",
   "                           "
   ]

--
illOCR = [
   " _ ",
   "| |",
   " _|"
  ]

--
illOCR2 = [
   " _ ",
   " _|",
   "|_|"
  ]

main :: IO()
main = hspec $ do
  describe "parse Digit" $ do
    it "should match each digit" $ do
       parseDigit one   `shouldBe` '1'
       parseDigit two   `shouldBe` '2'
       parseDigit three `shouldBe` '3'
       parseDigit four  `shouldBe` '4'
       parseDigit five  `shouldBe` '5'
       parseDigit six   `shouldBe` '6'
       parseDigit seven `shouldBe` '7'
       parseDigit eight `shouldBe` '8'
       parseDigit nine  `shouldBe` '9'
       parseDigit zero  `shouldBe` '0'

    it "should return '?' for illegible OCR" $ do
       parseDigit illOCR `shouldBe` '?'

  describe "parse single Entry" $ do
     it "each entry should be 4 lines, and each line 27 characters" $ do
          isEntryValid entry1 `shouldBe` True

     it "should be False if entry has only 3 lines" $ do
          isEntryValid entry_3_lines `shouldBe` False

     it "should be False if one line has 28 characters" $ do
          isEntryValid entry_28_chars `shouldBe` False

     it "should be False if last line has non space character" $ do
          isEntryValid entryLastLineNonSpace `shouldBe` False

     it "each of the first 3 lines only contains | or _" $ do
         isEntryValid entryHasEqualCharacter `shouldBe` False

     it "should parse entry1 successfully" $ do
        parseEntry entry1 `shouldBe` Just "123456789"
        parseEntry entry_3_lines `shouldBe` Nothing

  describe "parse multiple entries" $ do
    it "should return a list of 2 accounts for 2 entries" $ do
       parseEntries twoEntries `shouldBe` [Just "123456789", Just "123456789"]

    it "should return 1 invalid account for 1 invalid entry" $ do
       parseEntries twoEntriesWithInvalid `shouldBe` [Just "123456789", Just "1?3456789"]

  describe "calculate checksum" $ do
     it "should return 0 for account 3 4 5 8 8 2 8 6 5" $ do
        checksum "345882865" `shouldBe` 0

  describe "support error status" $ do
     it "should return success if passing the checksum" $ do
        validateAcccount "457508000" `shouldBe` (Right OK)

     it "should return ERR if failed in checksum check" $ do
        validateAcccount "664371495" `shouldBe` (Left ERR)

     it "support illegible check" $ do
        validateAcccount "86110??36" `shouldBe` (Left ILL)

  describe "support fix 1 bit error" $ do
      it "should return status AMB if multiple choices" $ do
         fixError "490067715" `shouldBe` (Left AMB)
      --
      it "should return the option if only one choice" $ do
         fixError "664371495" `shouldBe` (Right "664371485")


  describe "fix isIllegible error" $ do
     it "should find candidates for isIllegible OCR entry" $ do
        changeOCROptions (digits entryIllegibleDigit) `shouldMatchList` ["133456789","183456789","123456789"]
     --
     it "should return empty for not fixable OCR digit" $ do
        changeOCROptions (digits entryIllegibleNoOption) `shouldBe` []
     --
     it "should find candidates for 2 illegible OCR digits entry" $ do
        length (changeOCROptions (digits entry2IllegibleDigits)) `shouldBe` 9

     --
     it "should return success fix for isIllegible OCR entry" $ do
        fixOCRError (digits entryIllegibleDigit) `shouldBe` (Right "123456789")
     --
     it "should return ILL Status for not fixable OCR entry" $ do
        fixOCRError (digits entryIllegibleNoOption) `shouldBe` (Left ILL)
