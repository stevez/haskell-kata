
import BowlingGame

import Test.Hspec

main :: IO()
main = hspec $ do
 describe "Bowling Score" $ do
   it "should be the sum of the pins for no spair,no strike" $ do
     score "12345123451234512345" `shouldBe` 60

   it "is 90 for hearbreak" $ do
     score "9-9-9-9-9-9-9-9-9-9-" `shouldBe` 90

   it "has spare for every round" $ do
     score "5/5/5/5/5/5/5/5/5/5/5" `shouldBe` 150

   it "has 9-1 spare for every round" $ do
     score "9/9/9/9/9/9/9/9/9/9/9" `shouldBe` 190

   it "has 9-1 spare for every round, last throw is X" $ do
     score "9/9/9/9/9/9/9/9/9/9/X" `shouldBe` 191

   it "calculate perfect game" $ do
     score "XXXXXXXXXXXX" `shouldBe` 300

   it "perfect game but miss one pin" $ do
      score "XXXXXXXXXXX9" `shouldBe` 299

   it "perfect game but miss 2 short" $ do
       score "XXXXXXXXXX91" `shouldBe` 289
