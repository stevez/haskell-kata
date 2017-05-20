{-# LANGUAGE QuasiQuotes #-}

import Test.Hspec
import Control.Exception (evaluate)
import Text.RawString.QQ

import MinesSweeper

main :: IO()
main = hspec $ do
  describe "regular cases" $ do
    it "should solve a 4x4 case" $ do
       solve 4 4 ["*...","....",".*..","...."]  `shouldBe` ["*100","2210","1*10","1110"]

  describe "simple edge cases" $ do
    it "should solve simple cases" $ do
       solve 1 1 ["*"]  `shouldBe` ["*"]
       solve 1 2 ["*."]  `shouldBe`["*1"]
       solve 2 1 ["*", "."]  `shouldBe` ["*", "1"]
