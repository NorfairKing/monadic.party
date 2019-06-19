module AdditionSpec where

import Test.Hspec

spec :: Spec
spec = describe "addition" $ it "still makes sense" $ 1 + 1 `shouldBe` 2
