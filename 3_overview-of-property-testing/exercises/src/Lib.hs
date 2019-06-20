module Lib where

import QuickCheck

data IntOrString
  = I Int
  | S String
  deriving (Show, Eq)

data MyType =
  MyType
    { myBool :: Bool
    , myRational :: Rational
    }
  deriving (Show, Eq)

data Tree a
  = Leaf a
  | Branch (Tree a) (Tree a) (Tree a)
  deriving (Show, Eq)
