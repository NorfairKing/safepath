{-# LANGUAGE OverloadedStrings #-}
module Data.PathSpec (spec) where

import Test.Hspec
import Test.Validity
import Test.QuickCheck

import Control.Exception (evaluate)

import Data.Path.Internal
import Data.Path.Gen ()

uncheckedPath :: Gen FilePath
uncheckedPath = arbitrary

spec :: Spec
spec = do
    genSpec

    describe "constructValid" $ do
        it "produces valid paths when it succeeds" $ do
            validIfSucceeds constructValid

    describe "constructValidUnsafe" $ do
        it "produces valid paths when it does not crash" $ do
            forAll genUnchecked $ \inp ->
                if isValid inp
                then constructValidUnsafe (Just inp) `shouldBe` inp
                else evaluate (constructValidUnsafe (Just inp)) `shouldThrow` anyErrorCall

    describe "safeRelPath" $ do
        it "produces valid paths when it succeeds" $ do
            validIfSucceedsOnGen safeRelPath arbitrary

        it "fails to parse valid absolute paths" $ do
            pending
            -- forAll (genValid :: Gen AbsPath) $ \abspath ->
            --       safeRelPath (toFilePath abspath) `shouldBe` Nothing

        it "succeeds on these unit tests" $ do
            let shouldParseTo inp pieces exts
                    = safeRelPath inp
                    `shouldBe` Just (Path (map PathPiece pieces) (map Extension exts))
            shouldParseTo "test"
                ["test"] []
            shouldParseTo "test/file"
                ["test","file"] []
            shouldParseTo "test/file/path"
                ["test", "file", "path"] []
            shouldParseTo "directory/file.ext"
                ["directory","file"] ["ext"]

    describe "unsafeRelPathError" $ do
        it "behaves just like safeRelPath except for errors" $ do
            forAll uncheckedPath $ \fp ->
                case safeRelPath fp of
                    Nothing -> evaluate (unsafeRelPathError fp) `shouldThrow` anyErrorCall
                    Just res -> unsafeRelPathError fp `shouldBe` res


    describe "safeAbsPath" $ do
        it "produces valid paths when it succeeds" $ do
            validIfSucceedsOnGen safeRelPath arbitrary

        it "fails to parse valid relative paths" $ do
            pending
            -- forAll (genValid :: Gen RelPath) $ \relpath ->
            --       safeRelPath (toFilePath relpath) `shouldBe` Nothing

    describe "unsafeAbsPathError" $ do
        it "behaves just like safeRelPath except for errors" $ do
            forAll uncheckedPath $ \fp ->
                case safeRelPath fp of
                    Nothing -> evaluate (unsafeRelPathError fp) `shouldThrow` anyErrorCall
                    Just res -> unsafeRelPathError fp `shouldBe` res

    describe "toFilePath" $ do
        it "is the inverse of the succeeding runs of safeRelPath when starting with a fp" $ do
            forAll uncheckedPath $ \fp ->
                case safeRelPath fp of
                    Nothing -> return () -- Can happen
                    Just relpath -> toFilePath relpath `shouldBe` fp

        it "is the inverse of the succeeding runs of safeRelPath when starting with a valid relpath" $ do
            forAll genValid $ \relpath ->
                  safeRelPath (toFilePath relpath) `shouldBe` Just relpath

        it "is the inverse of the succeeding runs of safeAbsPath when starting with a fp" $ do
            forAll uncheckedPath $ \fp ->
                case safeAbsPath fp of
                    Nothing -> return () -- Can happen
                    Just relpath -> toFilePath relpath `shouldBe` fp

        it "is the inverse of the succeeding runs of safeRelPath when starting with a valid abspath" $ do
            forAll genValid $ \abspath ->
                  safeAbsPath (toFilePath abspath) `shouldBe` Just abspath

    describe "</>" $ do
        it "produces valid paths when it succeeds" $ do
            pending
            -- producesValidsOnValids2 (</>)

    describe "<.>" $ do
        it "produces valid paths when it succeeds" $ do
            pending
            -- producesValidsOnValids2 (<.>)

genSpec :: Spec
genSpec = describe "GenSpec" $ do
    arbitrarySpec   (Proxy :: Proxy (Path Absolute))
    genValiditySpec (Proxy :: Proxy (Path Absolute))
    arbitrarySpec   (Proxy :: Proxy (Path Relative))
    genValiditySpec (Proxy :: Proxy (Path Relative))
    arbitrarySpec   (Proxy :: Proxy PathPiece)
    genValiditySpec (Proxy :: Proxy PathPiece)
    arbitrarySpec   (Proxy :: Proxy Extension)
    genValiditySpec (Proxy :: Proxy Extension)






