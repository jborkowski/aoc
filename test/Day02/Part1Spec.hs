module Day02.Part1Spec where

import Day02.Part1 (result)
import Test.Hspec

spec :: Spec
spec =
  describe "Part One should" $ do
    it "return 3500 for 1st example" $ do
      (head . result $ [ 1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50 ]) `shouldBe` (3500 :: Int)
    it "return 2 for 2nd example" $ do
      (head . result $ [ 1, 0, 0, 0, 99 ]) `shouldBe` (2 :: Int)
    it "return 6 for 3rd example" $ do
      (head . drop 3 . result $ [ 2, 3, 0, 3, 99 ]) `shouldBe` (6 :: Int)
    it "return 9801 for 4th example" $ do
      (last . result $ [ 2, 4, 4, 5, 99, 0 ]) `shouldBe` (9801 :: Int)
    it "return 30 for 5th example" $ do
      (head . result $ [ 1, 1, 1, 4, 99, 5, 6, 0, 99 ]) `shouldBe` (30 :: Int)
