module Data.PathSpec (spec) where

import Test.Hspec
import Test.Validity

import Data.Path.Internal
import Data.Path.Gen ()

spec :: Spec
spec = do
    genSpec

    describe "constructValid" $ do
        it "produces valid paths when it succeeds" $ do
            validIfSucceeds constructValid

    describe "safeRelPath" $ do
        it "produces valid paths when it succeeds" $ do
            pending
            -- validIfSucceedsOnGen safeRelPath arbitrary

    describe "unsafeRelPathError" $ do
        it "produces valid paths when it succeeds" $ do
            pending
            -- producesValidsOnGen unsafeRelPathError arbitrary

    describe "safeAbsPath" $ do
        it "produces valid paths when it succeeds" $ do
            pending
            -- validIfSucceedsOnGen safeRelPath arbitrary

    describe "unsafeAbsPathError" $ do
        it "produces valid paths when it succeeds" $ do
            pending
            -- producesValidsOnGen unsafeRelPathError arbitrary

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






