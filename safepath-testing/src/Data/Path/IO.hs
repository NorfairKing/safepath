{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Path.IO where

import Data.Aeson
import qualified Data.Vector as V

import Data.Path.Internal

instance ToJSON AbsPath where
    toJSON (AbsPath ps lp es)
        = Array $ V.fromList [String "a", toJSON ps, toJSON lp, toJSON es]

instance FromJSON AbsPath where
    parseJSON (Array v) = do
        case V.toList v of
            [String "a", eps, elp, ees] -> AbsPath
                <$> parseJSON eps
                <*> parseJSON elp
                <*> parseJSON ees
            _ -> mempty
    parseJSON _ = mempty

instance ToJSON RelPath where
    toJSON (RelPath ps lp es)
        = Array $ V.fromList [String "r", toJSON ps, toJSON lp, toJSON es]

instance FromJSON RelPath where
    parseJSON (Array v) = do
        case V.toList v of
            [String "r", eps, elp, ees] -> RelPath
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

