module Data.PathSpec (spec) where

import Test.Hspec

import Data.Proxy

import Test.Validity

import Data.Path.Internal
import Data.Path.Gen ()

spec :: Spec
spec = do
    genSpec

genSpec :: Spec
genSpec = describe "GenSpec" $ do
    arbitrarySpec   (Proxy :: Proxy (Path Absolute))
    arbitrarySpec   (Proxy :: Proxy (Path Relative))
    -- genValiditySpec (Proxy :: Proxy (Path rel))






