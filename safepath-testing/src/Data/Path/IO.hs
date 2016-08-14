{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Path.IO where

import Data.Aeson
import qualified Data.Vector as V

import Data.Path.Internal

instance ToJSON   (Path Absolute) where
    toJSON (Path ps lp es) = Array $ V.fromList [String "a", toJSON ps, toJSON lp, toJSON es]

instance FromJSON (Path Absolute) where
    parseJSON (Array v) = do
        case V.toList v of
            [String "a", eps, elp, ees] -> Path
                <$> parseJSON eps
                <*> parseJSON elp
                <*> parseJSON ees
            _ -> mempty
    parseJSON _ = mempty

instance ToJSON   (Path Relative) where
    toJSON (Path ps lp es) = Array $ V.fromList [String "r", toJSON ps, toJSON lp, toJSON es]

instance FromJSON (Path Relative) where
    parseJSON (Array v) = do
        case V.toList v of
            [String "r", eps, elp, ees] -> Path
                <$> parseJSON eps
                <*> parseJSON elp
                <*> parseJSON ees
            _ -> mempty
    parseJSON _ = mempty

instance ToJSON   PathPiece
instance FromJSON PathPiece

instance ToJSON   LastPathPiece
instance FromJSON LastPathPiece

instance ToJSON   Extension
instance FromJSON Extension

