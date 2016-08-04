{-# LANGUAGE OverloadedStrings #-}
module Data.PathSpec (spec) where

import Test.Hspec
import Test.Validity
import Test.QuickCheck

import Control.Exception (evaluate)
import Control.Monad (forM_)

import Data.Path.Internal
import Data.Path.Gen
import Data.PathCases

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

        let works gen = forAll gen $ \(fp, path) -> safeRelPath fp `shouldBe` Just path
        it "succesfully correctly parses single-piece filepaths" $ do
            works genRelPathSinglePieceFilePath

        it "succesfully correctly parses filepaths without extensions" $ do
            works genRelPathNoExtensions

        it "succeeds on these black-box tests" $ do
            forM_ relativePathCases $ \(inp, path) ->
                safeRelPath inp `shouldBe` Just path

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

        let works gen = forAll gen $ \(fp, path) -> safeAbsPath fp `shouldBe` Just path
        it "succesfully correctly parses single-piece filepaths" $ do
            works genAbsPathSinglePieceFilePath

        it "succesfully correctly parses filepaths without extensions" $ do
            works genAbsPathNoExtensions

        it "succeeds on these black-box tests" $ do
            forM_ absolutePathCases $ \(inp, path) ->
                safeAbsPath inp `shouldBe` Just path

    describe "unsafeAbsPathError" $ do
        it "behaves just like safeRelPath except for errors" $ do
            forAll uncheckedPath $ \fp ->
                case safeRelPath fp of
                    Nothing -> evaluate (unsafeRelPathError fp) `shouldThrow` anyErrorCall
                    Just res -> unsafeRelPathError fp `shouldBe` res

    describe "toFilePath" $ do
        it "is the inverse of the succeeding runs of safeRelPath when starting with a fp" $ do
            pending
            -- forAll uncheckedPath $ \fp ->
            --     case safeRelPath fp of
            --         Nothing -> return () -- Can happen
            --         Just relpath -> toFilePath relpath `shouldBe` fp

        it "is the inverse of the succeeding runs of safeRelPath when starting with a valid relpath" $ do
            pending
            -- forAll genValid $ \relpath ->
            --       safeRelPath (toFilePath relpath) `shouldBe` Just relpath

        it "is the inverse of the succeeding runs of safeAbsPath when starting with a fp" $ do
            pending
            -- forAll uncheckedPath $ \fp ->
            --     case safeAbsPath fp of
            --         Nothing -> return () -- Can happen
            --         Just relpath -> toFilePath relpath `shouldBe` fp

        it "is the inverse of the succeeding runs of safeRelPath when starting with a valid abspath" $ do
            pending
            -- forAll genValid $ \abspath ->
            --       safeAbsPath (toFilePath abspath) `shouldBe` Just abspath

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






