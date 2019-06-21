--
--
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Data.Aeson
import GHC.Generics (Generic)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data User =
  User
    { userName :: UserName
    , userActivated :: Bool
    }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON User

instance ToJSON User

newtype UserName =
  UserName
    { userNameString :: String
    }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON UserName where
  parseJSON v = UserName <$> parseJSON v

instance ToJSON UserName where
  toJSON (UserName s) = toJSON s
