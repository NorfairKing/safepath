{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Path.Gen where

import Test.QuickCheck

import Data.GenValidity
import Data.GenValidity.Text

import qualified Data.Text as T

import Data.Path.Internal

instance Arbitrary AbsPath where
    arbitrary = genValid

instance GenValidity AbsPath where
    genUnchecked = AbsPath
        <$> genUnchecked
        <*> genUnchecked
        <*> genUnchecked

instance Arbitrary RelPath where
    arbitrary = genValid

instance GenValidity RelPath where
    genUnchecked = RelPath
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
    (fp, RelPath pp lp es) <- gen
    return ('/':fp, AbsPath pp lp es)

genRelPathSinglePieceFilePath :: Gen (FilePath, RelPath)
genRelPathSinglePieceFilePath = do
    piece <- genValid
    let fp = renderLastPiece piece
        path = RelPath [] (LastPathPiece $ PathPiece $ T.pack fp) []
    return (fp, path)

genAbsPathSinglePieceFilePath :: Gen (FilePath, AbsPath)
genAbsPathSinglePieceFilePath = toAbsPathGen genRelPathSinglePieceFilePath

genRelPathNoExtensions :: Gen (FilePath, RelPath)
genRelPathNoExtensions = do
    RelPath pieces lp _ <- genValid
    let path = RelPath pieces lp []
        fp = toRelFilePath path
    return (fp, path)

genAbsPathNoExtensions :: Gen (FilePath, AbsPath)
genAbsPathNoExtensions = toAbsPathGen genRelPathNoExtensions







