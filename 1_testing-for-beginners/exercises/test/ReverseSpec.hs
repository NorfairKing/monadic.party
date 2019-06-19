module ReverseSpec where

import Test.Hspec

import Reverse

spec :: Spec
spec =
  describe "revese" $ do
    it "does nothing to an empty list" $ rev [] `shouldBe` ([] :: [Int])
    it "does nothing to a list of 5" $ rev [5] `shouldBe` [5]
    it "reverses this list correctly" $ rev [1, 5, 4] `shouldBe` [4, 5, 1]
