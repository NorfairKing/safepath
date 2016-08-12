{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module TestCase where

import GHC.Generics
import Data.Data

import Test.QuickCheck
import Data.GenValidity
import Data.Aeson

import Data.Path.Internal
import Data.Path.IO ()
import Data.Path.Gen ()


data TestCase rel = TestCase FilePath (Path rel)
    deriving (Show, Eq, Generic, Data, Typeable)

instance FromJSON (TestCase Absolute)
instance ToJSON   (TestCase Absolute)

instance FromJSON (TestCase Relative)
instance ToJSON   (TestCase Relative)

instance Arbitrary (TestCase rel) where
    arbitrary = TestCase <$> arbitrary <*> genUnchecked





