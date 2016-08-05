{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
module Data.Path.Internal where

import Data.Monoid ((<>))
import Data.Typeable
import GHC.Generics
import Data.Maybe (isJust)
import Data.String (IsString(..))
import Data.List (intercalate)
import Data.Data

import Data.Text (Text)
import qualified Data.Text as T
import Safe

import Data.Validity

type AbsPath = Path Absolute
type RelPath = Path Relative

data Path rel
    = Path
    { pathPieces      :: [PathPiece]
    , pathLastPiece   :: LastPathPiece
    , pathExtensions  :: [Extension]
    } deriving (Show, Eq, Generic, Data, Typeable)

-- Choose nicer ways of printing if the context allows the compiler to figure
-- out what kind of path it is.

instance Show (Path Relative) where
    show = toRelFilePath

instance Show (Path Absolute) where
    show = toAbsFilePath

data Absolute = Absolute
    deriving (Generic, Data, Typeable)

data Relative = Relative
    deriving (Generic, Data, Typeable)

instance Validity (Path rel) where
    isValid Path{..}
        =  isValid pathPieces
        && isValid pathLastPiece
        && isValid pathExtensions

newtype PathPiece = PathPiece Text
    deriving (Show, Eq, Generic, Data, Typeable)

instance Validity PathPiece where
    isValid (PathPiece t) = not (T.null t) && not (containsSeparator t)

newtype LastPathPiece = LastPathPiece PathPiece
    deriving (Show, Eq, Generic, Data, Typeable)

instance Validity LastPathPiece where
    isValid (LastPathPiece pp@(PathPiece t)) = isValid pp && not (containsDot t)

newtype Extension = Extension Text
    deriving (Show, Eq, Generic, Data, Typeable)

instance Validity Extension where
    isValid (Extension t) = not (T.null t) && not (containsDot t) && not (containsSeparator t)

-- | ONLY for @OverloadedStrings@
instance IsString (Path Absolute) where
    fromString = unsafeAbsPathError

-- | ONLY for @OverloadedStrings@
instance IsString (Path Relative) where
    fromString = unsafeRelPathError

containsSatisfied :: (Char -> Bool) -> Text -> Bool
containsSatisfied func = isJust . T.find func

containsSeparator :: Text -> Bool
containsSeparator = containsSatisfied (== '/')

containsDot :: Text -> Bool
containsDot = containsSatisfied (== '.')


constructValid :: Path rel -> Maybe (Path rel)
constructValid p = if isValid p then Just p else Nothing

constructValidUnsafe :: Maybe (Path rel) -> Path rel
constructValidUnsafe Nothing = error "unable to make a path out of nothing"
constructValidUnsafe (Just p) =
    case constructValid p of
        Nothing -> error $ show p ++ " does not make for a valid Path."
        Just p -> p

safeRelPath :: FilePath -> Maybe RelPath
safeRelPath ('/':_) = Nothing
safeRelPath fp = do
    let rawPieces = filter (not . T.null) $ T.split (== '/') $ T.pack fp
    (firstPieces, lastRawPiece) <- unsnocMay rawPieces
    let rawExts = filter (not . T.null) $ T.split (== '.') lastRawPiece
    (lastPieceStr, safeExts) <- unconsMay rawExts
    let pieces = map PathPiece firstPieces
    let lastPiece = LastPathPiece $ PathPiece lastPieceStr
    let exts = map Extension safeExts
    return $ Path pieces lastPiece exts
  where
    unsnocMay :: [a] -> Maybe ([a], a)
    unsnocMay as = (,) <$> initMay as <*> lastMay as
    unconsMay :: [a] -> Maybe (a, [a])
    unconsMay [] = Nothing
    unconsMay (a:as) = Just (a, as)


unsafeRelPathError :: FilePath -> RelPath
unsafeRelPathError = constructValidUnsafe . safeRelPath

safeAbsPath :: FilePath -> Maybe AbsPath
safeAbsPath [] = Nothing
safeAbsPath ('/':fp) = unsafePathTypeCoerse <$> safeRelPath fp
safeAbsPath _ = Nothing

unsafeAbsPathError :: FilePath -> AbsPath
unsafeAbsPathError = constructValidUnsafe . safeAbsPath

toRelFilePath :: RelPath -> FilePath
toRelFilePath Path{..}
    =  intercalate "/" (map renderPiece $ pathPieces ++ [lpp])
    ++ renderExtensions pathExtensions
  where (LastPathPiece lpp) = pathLastPiece

toAbsFilePath :: AbsPath -> FilePath
toAbsFilePath = ('/':) . toRelFilePath . unsafePathTypeCoerse

lastPieceToPiece :: LastPathPiece -> PathPiece
lastPieceToPiece (LastPathPiece p) = p

renderPiece :: PathPiece -> String
renderPiece (PathPiece p) = T.unpack p

renderLastPiece :: LastPathPiece -> String
renderLastPiece = renderPiece . lastPieceToPiece

renderExtension :: Extension -> String
renderExtension (Extension e) = T.unpack e

renderExtensions :: [Extension] -> String
renderExtensions = intercalate "." . map renderExtension

combineLastAndExtensions :: LastPathPiece -> [Extension] -> PathPiece
combineLastAndExtensions (LastPathPiece (PathPiece lpp)) es
    = PathPiece $ lpp <> T.pack (renderExtensions es)

unsafePathTypeCoerse :: Path rel -> Path rel'
unsafePathTypeCoerse (Path pieces lastPiece exts) = Path pieces lastPiece exts

-- | If the first path has extensions, they will be appended to the last
-- pathpiece before concatenation
(</>) :: Path rel -> RelPath -> Path rel
(</>) p1 p2
    = Path
    { pathPieces = pathPieces p1 ++ [combineLastAndExtensions (pathLastPiece p1) (pathExtensions p1)] ++ pathPieces p2
    , pathLastPiece = pathLastPiece p2
    , pathExtensions = pathExtensions p2
    }

(<.>) :: Path rel -> Extension -> Path rel
(<.>) path extension
    = path
    { pathExtensions = pathExtensions path ++ [extension]
    }

