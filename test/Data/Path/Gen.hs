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
    genUnchecked = Path <$> genUnchecked <*> genUnchecked

instance Arbitrary PathPiece where
    arbitrary = genValid

instance GenValidity PathPiece where
    genUnchecked = PathPiece <$> genUncheckedText

instance Arbitrary Extension where
    arbitrary = genValid

instance GenValidity Extension where
    genUnchecked = Extension <$> genUncheckedText

toAbsPathGen :: Gen (FilePath, RelPath) -> Gen (FilePath, AbsPath)
toAbsPathGen gen = do
    (fp, path) <- gen
    return ('/':fp, unsafePathTypeCoerse path)

genValidLastPathPiece :: Gen PathPiece
genValidLastPathPiece = genValid `suchThat` isValidLastPiece

genRelPathSinglePieceFilePath :: Gen (FilePath, RelPath)
genRelPathSinglePieceFilePath = do
    piece <- genValidLastPathPiece
    let fp = renderPiece piece
        path = Path [PathPiece $ T.pack fp] []
    return (fp, path)

genAbsPathSinglePieceFilePath :: Gen (FilePath, AbsPath)
genAbsPathSinglePieceFilePath = toAbsPathGen genRelPathSinglePieceFilePath

genRelPathNoExtensions :: Gen (FilePath, RelPath)
genRelPathNoExtensions = do
    Path pieces _ <- genValid
    let path = Path pieces []
        fp = toFilePath path
    return (fp, path)

genAbsPathNoExtensions :: Gen (FilePath, AbsPath)
genAbsPathNoExtensions = toAbsPathGen genRelPathNoExtensions







