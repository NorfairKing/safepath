{-# LANGUAGE FlexibleInstances #-}
module Data.Path.Gen where

import Test.QuickCheck

import Data.GenValidity
import Data.GenValidity.Text

import Data.Path.Internal

instance Arbitrary (Path Absolute) where
    arbitrary = genValid

instance Arbitrary (Path Relative) where
    arbitrary = genValid

instance GenValidity (Path Absolute) where
    genUnchecked = Path <$> genListOf genUncheckedText <*> genListOf genUncheckedText

instance GenValidity (Path Relative) where
    genUnchecked = Path <$> genListOf genUncheckedText <*> genListOf genUncheckedText
