{-# LANGUAGE TypeApplications #-}

module LibSpec
  ( spec
  ) where

import Lib

import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

spec :: Spec
spec = do
  describe "User" $ do
    eqSpec @User
    ordSpec @User
    jsonSpec @User
  describe "UserName" $ do
    eqSpec @UserName
    ordSpec @UserName
    jsonSpec @UserName

instance GenUnchecked User

instance GenUnchecked UserName
