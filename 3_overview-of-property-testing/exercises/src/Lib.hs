{-# LANGUAGE InstanceSigs #-}

module Lib where

import Test.QuickCheck as Q
import Test.Validity

data IntOrString
  = I Int -- I :: Int -> IntOrString
  | S String
  deriving (Show, Eq)

-- elements :: [a] -> Gen a
-- arbitrary :: Arbitrary a => Gen a
-- oneof :: [Gen a] -> Gen a
-- suchThat :: Gen a -> (a -> Bool) -> Gen a
-- fmap  :: (a -> b) -> Gen a -> Gen b
-- (<$>) :: (a -> b) -> Gen a -> Gen b
instance Arbitrary IntOrString where
  arbitrary :: Q.Gen IntOrString
  arbitrary = oneof [I <$> arbitrary, S <$> arbitrary]
  shrink :: IntOrString -> [IntOrString]
  shrink ios =
    case ios of
      I i -> [I i' | i' <- shrink i] -- I <$> shrink i
      S s -> I 0 : [S s' | s' <- shrink s]

data MyType =
  MyType
    { myBool :: Bool
    , myRational :: Rational
    }
  deriving (Show, Eq)

instance Arbitrary MyType where
  arbitrary = MyType <$> arbitrary <*> arbitrary
  shrink (MyType b r) = [MyType b' r' | (b', r') <- shrink (b, r)]

data TTree
  = TNil
  | TBranch TTree TTree TTree
  deriving (Show, Eq)

instance Arbitrary TTree where
  arbitrary =
    sized $ \n ->
      case n of
        0 -> pure TNil
        _ -> do
          newSize <- upTo n
          (a, b, c) <- genSplit3 newSize
          TBranch <$> resize a arbitrary <*> resize b arbitrary <*> resize c arbitrary
