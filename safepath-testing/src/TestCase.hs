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


data RelTestCase = RelTestCase FilePath RelPath
    deriving (Show, Eq, Generic, Data, Typeable)

instance FromJSON RelTestCase
instance ToJSON   RelTestCase

instance Arbitrary RelTestCase where
    arbitrary = RelTestCase <$> arbitrary <*> genUnchecked

data AbsTestCase = AbsTestCase FilePath AbsPath
    deriving (Show, Eq, Generic, Data, Typeable)

instance FromJSON AbsTestCase
instance ToJSON   AbsTestCase

instance Arbitrary AbsTestCase where
    arbitrary = AbsTestCase <$> arbitrary <*> genUnchecked





