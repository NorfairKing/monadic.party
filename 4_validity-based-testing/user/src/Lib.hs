--
--
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Data.Aeson as JSON
import Data.Char as Char
import Data.Function (on)
import Data.List (find, sort)
import Data.Ord
import Data.Validity
import GHC.Generics (Generic)
import Path.IO
import System.Environment (getArgs)
import System.Exit (die)

-- stack test --copy-bins --file-watch --exec='user-exe'
someFunc :: IO ()
someFunc = do
  args <- getArgs
  case args of
    ["list"] -> listUsers
    ["add", uns] -> addUser uns
    ["activate", uns] -> activateUser uns
    _ -> die "Did not understand arguments."

listUsers :: IO ()
listUsers = do
  us <- loadUsers
  putStrLn $ unlines $ map show $ sort us

addUser :: String -> IO ()
addUser n =
  withUserName n $ \un ->
    withUsers $ \us -> do
      let u = User {userName = un, userActivated = False}
      pure $ u : us

activateUser :: String -> IO ()
activateUser n =
  withUserName n $ \un ->
    withUsers $ \us ->
      case find ((== un) . userName) us of
        Nothing -> die $ "User not found: " ++ userNameString un
        Just u -> do
          let u' = u {userActivated = True}
          let us' = u' : filter (/= u) us
          pure us'

withUserName :: String -> (UserName -> IO a) -> IO a
withUserName s func = do
  case prettyValidate $ UserName s of
    Left err -> die $ "Invalid username: " ++ err
    Right un -> func un

withUsers :: ([User] -> IO [User]) -> IO ()
withUsers func = do
  us <- loadUsers
  us' <- func us
  storeUsers us'

storeUsers :: [User] -> IO ()
storeUsers = JSON.encodeFile usersFile

-- fromMaybe [] <$> (forgivingAbsence $ fromMaybe [] <$>  JSON.decodeFileStrict usersFile)
loadUsers :: IO [User]
loadUsers = do
  mUs <- forgivingAbsence $ JSON.decodeFileStrict usersFile
  case mUs of
    Nothing -> pure []
    Just Nothing -> pure []
    Just (Just us) -> pure us

usersFile :: FilePath
usersFile = "users.json"

data User =
  User
    { userName :: UserName
    , userActivated :: Bool
    }
  deriving (Show, Eq, Generic)

instance Validity User

instance Ord User where
  compare u1 u2 =
    compare (Down (userActivated u1)) (Down (userActivated u2)) <>
    compare (userName u1) (userName u2)

instance FromJSON User

instance ToJSON User

newtype UserName =
  UserName
    { userNameString :: String
    }
  deriving (Show, Eq, Ord, Generic)

instance Validity UserName where
  validate (UserName cs) =
    decorateList cs $ \c -> declare "Is not whitespace" $ not (Char.isSpace c)

instance FromJSON UserName where
  parseJSON v = UserName <$> parseJSON v

instance ToJSON UserName where
  toJSON (UserName s) = toJSON s
