{-# LANGUAGE FlexibleInstances #-}
module Data.Path.Gen where

import Test.QuickCheck

import Data.GenValidity
import Data.GenValidity.Text

import Data.Path.Internal

instance Arbitrary (Path rel) where
    arbitrary = genValid

instance GenValidity (Path rel) where
    genUnchecked = Path <$> genUnchecked <*> genUnchecked

instance Arbitrary PathPiece where
    arbitrary = genValid

instance GenValidity PathPiece where
    genUnchecked = PathPiece <$> genUncheckedText

instance Arbitrary Extension where
    arbitrary = genValid

instance GenValidity Extension where
    genUnchecked = Extension <$> genUncheckedText
