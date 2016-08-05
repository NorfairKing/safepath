{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Path.Gen where

import Test.QuickCheck

import Data.GenValidity
import Data.GenValidity.Text

import qualified Data.Text as T

import Data.Path.Internal

instance Arbitrary (Path rel) where
    arbitrary = genValid

instance GenValidity (Path rel) where
    genUnchecked = Path
        <$> genUnchecked
        <*> genUnchecked
        <*> genUnchecked

instance Arbitrary PathPiece where
    arbitrary = genValid

instance GenValidity PathPiece where
    genUnchecked = PathPiece <$> genUncheckedText

instance Arbitrary LastPathPiece where
    arbitrary = genValid

instance GenValidity LastPathPiece where
    genUnchecked = LastPathPiece <$> genUnchecked

instance Arbitrary Extension where
    arbitrary = genValid

instance GenValidity Extension where
    genUnchecked = Extension <$> genUncheckedText

toAbsPathGen :: Gen (FilePath, RelPath) -> Gen (FilePath, AbsPath)
toAbsPathGen gen = do
    (fp, path) <- gen
    return ('/':fp, unsafePathTypeCoerse path)

genRelPathSinglePieceFilePath :: Gen (FilePath, RelPath)
genRelPathSinglePieceFilePath = do
    piece <- genValid
    let fp = renderLastPiece piece
        path = Path [] (LastPathPiece $ PathPiece $ T.pack fp) []
    return (fp, path)

genAbsPathSinglePieceFilePath :: Gen (FilePath, AbsPath)
genAbsPathSinglePieceFilePath = toAbsPathGen genRelPathSinglePieceFilePath

genRelPathNoExtensions :: Gen (FilePath, RelPath)
genRelPathNoExtensions = do
    Path pieces lp _ <- genValid
    let path = Path pieces lp []
        fp = toRelFilePath path
    return (fp, path)

genAbsPathNoExtensions :: Gen (FilePath, AbsPath)
genAbsPathNoExtensions = toAbsPathGen genRelPathNoExtensions







