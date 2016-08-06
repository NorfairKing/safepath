{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE IncoherentInstances #-}
module Data.Path.Internal where

import Data.Monoid ((<>))
import Data.Typeable
import GHC.Generics
import Data.Maybe (isJust, fromMaybe)
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
        && (not (T.null lt) || (null pathPieces && null pathExtensions))
      where
        (LastPathPiece lt) = pathLastPiece

newtype PathPiece = PathPiece Text
    deriving (Show, Eq, Generic, Data, Typeable)

instance Validity PathPiece where
    isValid (PathPiece t) = not (T.null t) && not (containsSeparator t)

newtype LastPathPiece = LastPathPiece Text
    deriving (Show, Eq, Generic, Data, Typeable)

instance Validity LastPathPiece where
    isValid (LastPathPiece t) = not (containsSeparator t) && not (containsDot t)

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

emptyLastPathPiece :: LastPathPiece -> Bool
emptyLastPathPiece (LastPathPiece "") = True
emptyLastPathPiece _ = False

emptyPath :: Path rel -> Bool
emptyPath (Path [] (LastPathPiece "") []) = True
emptyPath _ = False

safeRelPath :: FilePath -> Maybe RelPath
safeRelPath [] = Nothing
safeRelPath ['.'] = Just $ Path [] (LastPathPiece "") []
safeRelPath ('/':_) = Nothing
safeRelPath fp = do
    let rawPieces = filter (not . T.null) $ T.split (== '/') $ T.pack fp
    (firstPieces, lastRawPiece) <- unsnocMay rawPieces
    let rawExts = filter (not . T.null) $ T.split (== '.') lastRawPiece
    (lastPieceStr, safeExts) <- unconsMay rawExts
    let pieces = map PathPiece firstPieces
    let lastPiece = LastPathPiece lastPieceStr
    let exts = map Extension safeExts
    return $ Path pieces lastPiece exts
  where
    unsnocMay :: [a] -> Maybe ([a], a)
    unsnocMay as = (,) <$> initMay as <*> lastMay as
    unconsMay :: [a] -> Maybe (a, [a])
    unconsMay [] = Nothing
    unconsMay (a:as) = Just (a, as)

unsafeRelPathError :: FilePath -> RelPath
unsafeRelPathError
    = constructValidUnsafe
    . fromMaybe (error "Invalid path")
    . safeRelPath

safeAbsPath :: FilePath -> Maybe AbsPath
safeAbsPath [] = Nothing
safeAbsPath ['/'] = Just $ Path [] (LastPathPiece "") []
safeAbsPath ('/':fp) = unsafePathTypeCoerse <$> safeRelPath fp
safeAbsPath _ = Nothing

unsafeAbsPathError :: FilePath -> AbsPath
unsafeAbsPathError
    = constructValidUnsafe
    . fromMaybe (error "Invalid path")
    . safeAbsPath

toRelFilePath :: RelPath -> FilePath
toRelFilePath (Path [] (LastPathPiece "") []) = "."
toRelFilePath Path{..}
    =  intercalate "/" (map renderPiece pathPieces ++ [renderLastPiece pathLastPiece])
    ++ renderExtensions pathExtensions

toAbsFilePath :: AbsPath -> FilePath
toAbsFilePath (Path [] (LastPathPiece "") []) = "/"
toAbsFilePath p = ('/':) . toRelFilePath . unsafePathTypeCoerse $ p

renderPiece :: PathPiece -> String
renderPiece (PathPiece p) = T.unpack p

renderLastPiece :: LastPathPiece -> String
renderLastPiece (LastPathPiece p) = T.unpack p

renderExtension :: Extension -> String
renderExtension (Extension e) = T.unpack e

renderExtensions :: [Extension] -> String
renderExtensions [] = ""
renderExtensions es = "." ++ intercalate "." (map renderExtension es)

combineLastAndExtensions :: LastPathPiece -> [Extension] -> PathPiece
combineLastAndExtensions (LastPathPiece lpp) es
    = PathPiece $ lpp <> T.pack (renderExtensions es)

unsafePathTypeCoerse :: Path rel -> Path rel'
unsafePathTypeCoerse (Path pieces lastPiece exts) = Path pieces lastPiece exts



-- | If the first path has extensions, they will be appended to the last
-- pathpiece before concatenation
(</>) :: Path rel -> RelPath -> Path rel
(</>) p1 p2
    | emptyPath p1 && emptyPath p2 = p1
    | emptyPath p2 = p1
    | emptyPath p1 = unsafePathTypeCoerse p2
    | otherwise = Path
        { pathPieces = pathPieces p1 ++ [combineLastAndExtensions (pathLastPiece p1) (pathExtensions p1)] ++ pathPieces p2
        , pathLastPiece = pathLastPiece p2
        , pathExtensions = pathExtensions p2
        }

(<.>) :: Path rel -> Extension -> Path rel
(<.>) path extension
    | emptyPath path = path
    | otherwise = path
    { pathExtensions = pathExtensions path ++ [extension]
    }

