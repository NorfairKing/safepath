{-# LANGUAGE OverloadedStrings #-}
module Data.PathSpec (spec) where

import Test.Hspec
import Test.Validity
import Test.QuickCheck

import Control.Exception (evaluate)
import Control.Monad (forM_, void, when)
import Control.DeepSeq (deepseq)
import Data.Typeable
import Data.Proxy
import Data.Data

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8

import Data.Aeson
import System.Directory (doesFileExist)

import Data.Path.Internal
import Data.Path.Gen
import Data.Path.IO ()
import Data.RegressionTests
import TestCase

uncheckedPath :: Gen FilePath
uncheckedPath = arbitrary

spec :: Spec
spec = do
    genSpec
    blackboxSpec

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

        it "succeeds on these regression tests" $ do
            forM_ absolutePathCases $ \(inp, path) ->
                safeAbsPath inp `shouldBe` Just path

        it "fails on these regression tests" $ do
            forM_ invalidAbsolutePaths $ \inp ->
                safeAbsPath inp `shouldBe` Nothing

    describe "unsafeAbsPathError" $ do
        it "behaves just like safeRelPath except for errors" $ do
            forAll uncheckedPath $ \fp ->
                case safeRelPath fp of
                    Nothing -> evaluate (unsafeRelPathError fp) `shouldThrow` anyErrorCall
                    Just res -> unsafeRelPathError fp `shouldBe` res

    describe "toRelFilePath" $ do
        it "succeeds on these regression tests" $ do
            forM_ relativePathCases $ \(inp, path) ->
                toRelFilePath path `shouldBe` inp

        it "fails on these regression tests" $ do
            forM_ invalidRelativePaths $ \inp ->
                safeRelPath inp `shouldBe` Nothing

        let works gen = forAll gen $ \(fp, path) -> toRelFilePath path `shouldBe` fp
        it "succesfully correctly outputs single-piece relative filepaths as generated" $ do
            works genRelPathSinglePieceFilePath

        it "succesfully correctly outputs relative filepaths without extensions as generated" $ do
            works genRelPathNoExtensions

        it "is the inverse of the succeeding runs of safeRelPath when starting with a single-piece fp" $ do
            inverseFunctionsIfSecondSucceedsOnGen toRelFilePath safeRelPath $ snd <$> genRelPathSinglePieceFilePath

        it "is the inverse of the succeeding runs of safeRelPath when starting with a fp without extensions" $ do
            inverseFunctionsIfSecondSucceedsOnGen toRelFilePath safeRelPath $ snd <$> genRelPathNoExtensions

        it "is the inverse of the succeeding runs of safeRelPath when starting with a fp" $ do
            pending
            -- inverseFunctionsIfSecondSucceeds toRelFilePath safeRelPath

        it "is the inverse of the succeeding runs of safeRelPath when starting with a single-piece valid relpath" $ do
            inverseFunctionsIfFirstSucceedsOnGen safeRelPath toRelFilePath $ fst <$> genRelPathSinglePieceFilePath

        it "is the inverse of the succeeding runs of safeRelPath when starting with a valid relpath without extensions" $ do
            inverseFunctionsIfFirstSucceedsOnGen safeRelPath toRelFilePath $ fst <$> genRelPathNoExtensions

        it "is the inverse of the succeeding runs of safeRelPath when starting with a valid relpath" $ do
            pending
            -- inverseFunctionsIfFirstSucceedsOnGen safeRelPath toRelFilePath arbitrary

    describe "toAbsFilePath" $ do
        it "succeeds on these regression tests" $ do
            forM_ absolutePathCases $ \(inp, path) ->
                toAbsFilePath path `shouldBe` inp

        let works gen = forAll gen $ \(fp, path) -> toAbsFilePath path `shouldBe` fp
        it "succesfully correctly outputs single-piece absolute filepaths as generated" $ do
            works genAbsPathSinglePieceFilePath

        it "succesfully correctly outputs absolute filepaths without extensions as generated" $ do
            works genAbsPathNoExtensions

        it "is the inverse of the succeeding runs of safeAbsPath when starting with a single-piece fp" $ do
            inverseFunctionsIfSecondSucceedsOnGen toAbsFilePath safeAbsPath $ snd <$> genAbsPathSinglePieceFilePath

        it "is the inverse of the succeeding runs of safeAbsPath when starting with a fp without extensions" $ do
            inverseFunctionsIfSecondSucceedsOnGen toAbsFilePath safeAbsPath $ snd <$> genAbsPathNoExtensions

        it "is the inverse of the succeeding runs of safeAbsPath when starting with a fp" $ do
            pending
            -- inverseFunctionsIfSecondSucceeds toAbsFilePath safeAbsPath

        it "is the inverse of the succeeding runs of safeAbsPath when starting with a single-piece valid abspath" $ do
            inverseFunctionsIfFirstSucceedsOnGen safeAbsPath toAbsFilePath $ fst <$> genAbsPathSinglePieceFilePath

        it "is the inverse of the succeeding runs of safeAbsPath when starting with a valid abspath without extensions" $ do
            inverseFunctionsIfFirstSucceedsOnGen safeAbsPath toAbsFilePath $ fst <$> genAbsPathNoExtensions

        it "is the inverse of the succeeding runs of safeRelPath when starting with a valid abspath" $ do
            pending
            -- inverseFunctionsIfFirstSucceedsOnGen safeAbsPath toAbsFilePath arbitrary

    describe "combineLastAndExtensions" $ do
        it "produces valid pathpieces on valids" $ do
            producesValidsOnGens2 combineLastAndExtensions (genValid `suchThat` (not . emptyLastPathPiece)) genValid

    describe "</>" $ do
        it "produces valid paths when it succeeds" $ do
            producesValidsOnValids2 (</>)

    describe "<.>" $ do
        it "produces valid paths when it succeeds" $ do
            producesValidsOnValids2 (<.>)

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

    customJSONSanity (Proxy :: Proxy (TestCase Absolute))
    customJSONSanity (Proxy :: Proxy (TestCase Relative))
    customJSONSanity (Proxy :: Proxy (Path Absolute))
    customJSONSanity (Proxy :: Proxy (Path Relative))
    customJSONSanity (Proxy :: Proxy LastPathPiece)
    customJSONSanity (Proxy :: Proxy PathPiece)
    customJSONSanity (Proxy :: Proxy Extension)

customJSONSanity
    :: (FromJSON a, ToJSON a, Arbitrary a, Show a, Eq a, Data a, Typeable a)
    => Proxy a
    -> Spec
customJSONSanity t = do
    let name = nameOf t
    describe ("JSON " ++ name) $ do
        it "never fails to encode" $ do
            property $ \a ->
                void (evaluate (deepseq (encode (a `asProxyTypeOf` t)))) `shouldReturn` ()

        it "ensures that encode and decode are inverses" $ do
            property $ \a ->
                decode (encode (a `asProxyTypeOf` t)) `shouldBe` Just a

nameOf :: Typeable a => Proxy a -> String
nameOf proxy =
    let (_, [ty]) = splitTyConApp $ typeOf proxy
    in show ty

blackboxSpec :: Spec
blackboxSpec = describe "Black-box tests" $ do
    describe "Parse tests" $ withExistingContentsLB "data/cases.txt" $ \lbs ->
        forM_ (LB8.lines lbs) $ \line ->
            case decode line of
                Just (TestCase fp relpath) ->
                    it fp $ safeRelPath fp `shouldBe` Just relpath
                Nothing ->
                    case decode line of
                        Just (TestCase fp abspath) ->
                            it fp $ safeAbsPath fp `shouldBe` Just abspath
                        Nothing -> return ()

    describe "Render tests" $ withExistingContentsLB "data/cases.txt" $ \lbs ->
        forM_ (LB8.lines lbs) $ \line ->
            case decode line of
                Just (TestCase fp relpath) ->
                    it fp $ toRelFilePath relpath `shouldBe` fp
                Nothing ->
                    case decode line of
                        Just (TestCase fp abspath) ->
                            it fp $ toAbsFilePath abspath `shouldBe` fp
                        Nothing -> return ()


withExisting :: FilePath -> Spec -> Spec
withExisting fp func = do
    exists <- runIO $ doesFileExist fp
    when exists func

-- | Sets up an expectation based on the contents of the given file, if the file exists.
-- (Text version)
withExistingContentsLB :: FilePath -> (LB.ByteString -> Spec) -> Spec
withExistingContentsLB fp func = do
    withExisting fp $ runIO (LB.readFile fp) >>= func

