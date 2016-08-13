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

    describe "combineLastAndExtensions" $ do
        it "produces valid pathpieces on valids" $ do
            producesValidsOnGens2 combineLastAndExtensions
                (genValid `suchThat` (not . isEmptyLastPathPiece)) genValid

    describe "splitPiece" $ do
        it "produces valid splits on valids" $ do
            producesValidsOnValids splitPiece

    describe "relpath" $ do
        it "produces valid paths when it succeeds" $ do
            validIfSucceedsOnGen relpath arbitrary

        it "fails to parse valid absolute paths" $ do
            forAll genValid $ \abspath ->
                relpath (toAbsFilePath abspath) `shouldBe` Nothing

        let works gen = forAll gen $ \(fp, path) -> relpath fp `shouldBe` Just path
        it "succesfully correctly parses single-piece filepaths as generated" $ do
            works genRelPathSinglePieceFilePath

        it "succesfully correctly parses filepaths without extensions as generated" $ do
            works genRelPathNoExtensions

        it "succeeds on these black-box tests" $ do
            forM_ relativePathCases $ \(inp, path) ->
                relpath inp `shouldBe` Just path

    describe "abspath" $ do
        it "produces valid paths when it succeeds" $ do
            validIfSucceedsOnGen relpath arbitrary

        it "fails to parse valid relative paths" $ do
            forAll genValid $ \relpath ->
                abspath (toRelFilePath relpath) `shouldBe` Nothing

        let works gen = forAll gen $ \(fp, path) -> abspath fp `shouldBe` Just path
        it "succesfully correctly parses single-piece filepaths as generated" $ do
            works genAbsPathSinglePieceFilePath

        it "succesfully correctly parses filepaths without extensions as generated" $ do
            works genAbsPathNoExtensions

        it "succeeds on these regression tests" $ do
            forM_ absolutePathCases $ \(inp, path) ->
                abspath inp `shouldBe` Just path

        it "fails on these regression tests" $ do
            forM_ invalidAbsolutePaths $ \inp ->
                abspath inp `shouldBe` Nothing

    describe "ext" $ do
        it "produces valid paths when it succeeds" $ do
            validIfSucceedsOnGen ext uncheckedPath

    describe "ground" $ do
        it "produces valid paths when it succeeds if the first argument is the empty path" $ do
            validIfSucceedsOnGens2 ground (pure emptyPath) uncheckedPath

        it "produces valid paths when it succeeds" $ do
            validIfSucceedsOnGens2 ground genValid uncheckedPath

    describe "unsafeRelPathError" $ do
        it "behaves just like relpath except for errors" $ do
            forAll uncheckedPath $ \fp ->
                case relpath fp of
                    Nothing -> evaluate (unsafeRelPathError fp) `shouldThrow` anyErrorCall
                    Just res -> unsafeRelPathError fp `shouldBe` res

    describe "unsafeAbsPathError" $ do
        it "behaves just like relpath except for errors" $ do
            forAll uncheckedPath $ \fp ->
                case relpath fp of
                    Nothing -> evaluate (unsafeRelPathError fp) `shouldThrow` anyErrorCall
                    Just res -> unsafeRelPathError fp `shouldBe` res

    describe "unsafeExtError" $ do
        it "behaves just like ext except for errors" $ do
            forAll uncheckedPath $ \fp ->
                case ext fp of
                    Nothing -> evaluate (unsafeExtError fp) `shouldThrow` anyErrorCall
                    Just res -> unsafeExtError fp `shouldBe` res

    describe "toRelFilePath" $ do
        it "succeeds on these regression tests" $ do
            forM_ relativePathCases $ \(inp, path) ->
                toRelFilePath path `shouldBe` inp

        it "fails on these regression tests" $ do
            forM_ invalidRelativePaths $ \inp ->
                relpath inp `shouldBe` Nothing

        let works gen = forAll gen $ \(fp, path) -> toRelFilePath path `shouldBe` fp
        it "succesfully correctly outputs single-piece relative filepaths as generated" $ do
            works genRelPathSinglePieceFilePath

        it "succesfully correctly outputs relative filepaths without extensions as generated" $ do
            works genRelPathNoExtensions

        it "is the inverse of the succeeding runs of relpath when starting with a single-piece fp" $ do
            inverseFunctionsIfSecondSucceedsOnGen toRelFilePath relpath $ snd <$> genRelPathSinglePieceFilePath

        it "is the inverse of the succeeding runs of relpath when starting with a fp without extensions" $ do
            inverseFunctionsIfSecondSucceedsOnGen toRelFilePath relpath $ snd <$> genRelPathNoExtensions

        it "is the inverse of the succeeding runs of relpath when starting with a single-piece valid relpath" $ do
            inverseFunctionsIfFirstSucceedsOnGen relpath toRelFilePath $ fst <$> genRelPathSinglePieceFilePath

        it "is the inverse of the succeeding runs of relpath when starting with a valid relpath without extensions" $ do
            inverseFunctionsIfFirstSucceedsOnGen relpath toRelFilePath $ fst <$> genRelPathNoExtensions

    describe "toAbsFilePath" $ do
        it "succeeds on these regression tests" $ do
            forM_ absolutePathCases $ \(inp, path) ->
                toAbsFilePath path `shouldBe` inp

        let works gen = forAll gen $ \(fp, path) -> toAbsFilePath path `shouldBe` fp
        it "succesfully correctly outputs single-piece absolute filepaths as generated" $ do
            works genAbsPathSinglePieceFilePath

        it "succesfully correctly outputs absolute filepaths without extensions as generated" $ do
            works genAbsPathNoExtensions

        it "is the inverse of the succeeding runs of abspath when starting with a single-piece fp" $ do
            inverseFunctionsIfSecondSucceedsOnGen toAbsFilePath abspath $ snd <$> genAbsPathSinglePieceFilePath

        it "is the inverse of the succeeding runs of abspath when starting with a fp without extensions" $ do
            inverseFunctionsIfSecondSucceedsOnGen toAbsFilePath abspath $ snd <$> genAbsPathNoExtensions

        it "is the inverse of the succeeding runs of abspath when starting with a single-piece valid abspath" $ do
            inverseFunctionsIfFirstSucceedsOnGen abspath toAbsFilePath $ fst <$> genAbsPathSinglePieceFilePath

        it "is the inverse of the succeeding runs of abspath when starting with a valid abspath without extensions" $ do
            inverseFunctionsIfFirstSucceedsOnGen abspath toAbsFilePath $ fst <$> genAbsPathNoExtensions

    describe "takeExtensions" $ do
        it "produces lists of extensions" $ do
            producesValidsOnValids takeExtensions

        it "produces the second element of the result of splitExtensions" $ do
            equivalent takeExtensions (snd . splitExtensions)

        it "finds the extensions set by replaceExtensionss for nonempty paths" $ do
            forAll (genUnchecked `suchThat` (not . isEmptyPath)) $ \path ->
                forAll genUnchecked $ \es ->
                    takeExtensions (replaceExtensionss path es) `shouldBe` es


    describe "replaceExtension" $ do
        it "produces valid paths if the first argument is the empty path" $ do
            producesValidsOnGens2 replaceExtension (pure emptyPath) genValid

        it "produces valid paths" $ do
            producesValidsOnValids2 replaceExtension

        it "is equivalent to addExtesion after dropExtension" $ do
            equivalent2 replaceExtension (\path ex -> addExtension (dropExtension path) ex)

    describe "replaceExtensions" $ do
        it "produces valid paths if the first argument is the empty path" $ do
            producesValidsOnGens2 replaceExtensions (pure emptyPath) genValid

        it "produces valid paths" $ do
            producesValidsOnValids2 replaceExtensions

    describe "replaceExtensionss" $ do
        it "produces valid paths if the first argument is the empty path" $ do
            producesValidsOnGens2 replaceExtensionss (pure emptyPath) genValid

        it "produces valid paths" $ do
            producesValidsOnValids2 replaceExtensionss

    describe "-<.>" $ do
        it "behaves exactly like replaceExtension" $ do
            equivalent2 replaceExtension (-<.>)

    describe "dropExtension" $ do
        it "produces valid paths" $ do
            producesValidsOnValids dropExtension

    describe "dropExtensions" $ do
        it "produces valid paths" $ do
            producesValidsOnValids dropExtensions

        it "is equivalent to producing the first element of the result of splitExtensions" $ do
            equivalent dropExtensions (fst . splitExtensions)

    describe "addExtension" $ do
        it "produces valid paths" $ do
            producesValidsOnValids2 addExtension

    describe "addExtensions" $ do
        it "produces valid paths" $ do
            producesValidsOnValids2 addExtension

    describe "<.>" $ do
        it "behaves exactly like addExtension" $ do
            equivalent2 addExtension (<.>)

    describe "stripExtension" $ do
        it "produces valid paths" $ do
            producesValidsOnValids2 stripExtension

    describe "stripExtensions" $ do
        it "produces valid paths" $ do
            producesValidsOnValids2 stripExtensions

    describe "splitExtension" $ do
        it "produces tuples of valid paths and a list of valid extensions" $ do
            producesValidsOnValids splitExtensions

        it "is the 'inverse' of addExtensions" $ do
            forAll genValid $ \path -> do
                let (stripped, exts) = splitExtensions path
                addExtensions stripped exts `shouldBe` path

    describe "hasExtension" $ do
        it "is equivalent to null after takeExtension" $ do
            equivalentOnValid hasExtension (not . null . takeExtensions)

    describe "replaceFileNameExact" $ do
        it "produces valid paths if the first argument is the empty path" $ do
            producesValidsOnGens2 replaceFileNameExact (pure emptyPath) genValid

        it "produces valid paths" $ do
            producesValidsOnValids2 replaceFileNameExact

    describe "replaceFileName" $ do
        it "produces valid paths if the first argument is the empty path" $ do
            producesValidsOnGens2 replaceFileName (pure emptyPath) genValid

        it "produces valid paths" $ do
            producesValidsOnValids2 replaceFileName

    describe "dropFileName" $ do
        it "produces valid paths" $ do
            producesValidsOnValids dropFileName

    describe "takeBaseName" $ do
        it "produces valid last pieces" $ do
            producesValidsOnValids takeBaseName

    describe "replaceBaseNameExact" $ do
        it "produces valid paths if the first argument is the empty path" $ do
            producesValidsOnGens2 replaceBaseNameExact (pure emptyPath) genValid

        it "produces valid paths if the second argument is the empty last path piece" $ do
            producesValidsOnGens2 replaceBaseNameExact genValid (pure emptyLastPathPiece)

        it "produces valid Maybe paths" $ do
            producesValidsOnValids2 replaceBaseNameExact

    describe "replaceBaseName" $ do
        it "produces valid paths if the first argument is the empty path" $ do
            producesValidsOnGens2 replaceBaseName (pure emptyPath) genValid

        it "produces valid paths if the second argument is the empty last path piece" $ do
            producesValidsOnGens2 replaceBaseName genValid (pure emptyLastPathPiece)

        it "produces valid paths" $ do
            producesValidsOnValids2 replaceBaseName

    describe "replaceDirectory" $ do
        it "produces valid paths if the first argument is the empty path" $ do
            producesValidsOnGens2 replaceDirectory (pure emptyPath) genValid

        it "produces valid paths if the second argument is the empty path" $ do
            producesValidsOnGens2 replaceDirectory genValid (pure emptyPath)

        it "produces valid paths" $ do
            producesValidsOnValids2 replaceDirectory

    describe "combine" $ do
        it "produces valid paths" $ do
            producesValidsOnValids2 (</>)

        it "is an associative operation" $ do
            associativeOnValids (</>)

        it "Has a left identity: the empty path" $ do
            leftIdentityOnValid (</>) emptyPath

        it "Has a right identity: the empty path" $ do
            rightIdentityOnValid (</>) emptyPath

    describe "</>" $ do
        it "is equivalent to combine" $ do
            equivalent2 combine (</>)

    describe "splitPath" $ do
        it "produces lists of valid path pieces" $ do
            producesValidsOnValids splitPath

    describe "joinPath" $ do
        it "produces valid paths" $ do
            producesValidsOnValids joinPath

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
    describe "Parse tests" $ withExistingContentsLB "data/cases.txt" $ \lbs -> do
        let ls = LB8.lines lbs
            nr = length ls
        it ("passes all" ++ show nr ++ "black-box parse tests") $ do
            forM_ ls $ \line ->
                case decode line of
                    Just (TestCase fp rp) ->
                        relpath fp `shouldBe` Just rp
                    Nothing ->
                        case decode line of
                            Just (TestCase fp ap) ->
                                abspath fp `shouldBe` Just ap
                            Nothing -> return ()

    describe "Render tests" $ withExistingContentsLB "data/cases.txt" $ \lbs -> do
        let ls = LB8.lines lbs
            nr = length ls
        it ("passes all" ++ show nr ++ "black-box render tests") $ do
            forM_ ls $ \line ->
                case decode line of
                    Just (TestCase fp relpath) ->
                        toRelFilePath relpath `shouldBe` fp
                    Nothing ->
                        case decode line of
                            Just (TestCase fp abspath) ->
                                toAbsFilePath abspath `shouldBe` fp
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

