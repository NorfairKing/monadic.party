module SortSpec where

import Test.Hspec

import Sort

spec :: Spec
spec =
  describe "revese" $ do
    it "does nothing to an empty list" $ sor [] `shouldBe` ([] :: [Int])
    it "does nothing to a list of 5" $ sor [5] `shouldBe` [5]
    it "reverses this list correctly" $ sor [1, 5, 4] `shouldBe` [1, 4, 5]
