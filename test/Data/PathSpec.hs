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
            forAll genValid $ \abspath ->
                safeRelPath (toAbsFilePath abspath) `shouldBe` Nothing

        let works gen = forAll gen $ \(fp, path) -> safeRelPath fp `shouldBe` Just path
        it "succesfully correctly parses single-piece filepaths as generated" $ do
            works genRelPathSinglePieceFilePath

        it "succesfully correctly parses filepaths without extensions as generated" $ do
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
            forAll genValid $ \relpath ->
                safeAbsPath (toRelFilePath relpath) `shouldBe` Nothing

        let works gen = forAll gen $ \(fp, path) -> safeAbsPath fp `shouldBe` Just path
        it "succesfully correctly parses single-piece filepaths as generated" $ do
            works genAbsPathSinglePieceFilePath

        it "succesfully correctly parses filepaths without extensions as generated" $ do
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
            -- inverseFunctionsIfSecondSucceeds toRelFilePath safeRelPath

        it "is the inverse of the succeeding runs of safeRelPath when starting with a valid relpath" $ do
            pending
            -- inverseFunctionsIfFirstSucceedsOnGen safeRelPath toRelFilePath arbitrary

        it "is the inverse of the succeeding runs of safeAbsPath when starting with a fp" $ do
            pending
            -- inverseFunctionsIfSecondSucceeds toAbsFilePath safeAbsPath

        it "is the inverse of the succeeding runs of safeRelPath when starting with a valid abspath" $ do
            pending
            -- inverseFunctionsIfFirstSucceedsOnGen safeAbsPath toAbsFilePath arbitrary

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
    arbitrarySpec   (Proxy :: Proxy LastPathPiece)
    genValiditySpec (Proxy :: Proxy LastPathPiece)
    arbitrarySpec   (Proxy :: Proxy Extension)
    genValiditySpec (Proxy :: Proxy Extension)






