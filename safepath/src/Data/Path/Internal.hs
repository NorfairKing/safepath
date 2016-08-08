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

data Absolute = Absolute
    deriving (Generic, Data, Typeable)

data Relative = Relative
    deriving (Generic, Data, Typeable)

-- Choose nicer ways of printing if the context allows the compiler to figure
-- out what kind of path it is.

instance Show (Path Relative) where
    show = toRelFilePath

instance Show (Path Absolute) where
    show = toAbsFilePath

-- | ONLY for @OverloadedStrings@
instance IsString (Path Absolute) where
    fromString = unsafeAbsPathError

-- | ONLY for @OverloadedStrings@
instance IsString (Path Relative) where
    fromString = unsafeRelPathError

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

instance IsString Extension where
    fromString = unsafeExt

instance Validity Extension where
    isValid (Extension t) = not (T.null t) && not (containsDot t) && not (containsSeparator t)

containsSatisfied :: (Char -> Bool) -> Text -> Bool
containsSatisfied func = isJust . T.find func

containsSeparator :: Text -> Bool
containsSeparator = containsSatisfied (== '/')

containsDot :: Text -> Bool
containsDot = containsSatisfied (== '.')

emptyLastPathPiece :: LastPathPiece -> Bool
emptyLastPathPiece (LastPathPiece "") = True
emptyLastPathPiece _ = False

emptyPath :: Path rel
emptyPath = (Path [] (LastPathPiece "") [])

isEmptyPath :: Path rel -> Bool
isEmptyPath p = p == emptyPath

relpath :: FilePath -> Maybe RelPath
relpath [] = Nothing
relpath ['.'] = Just emptyPath
relpath ('/':_) = Nothing
relpath fp = do
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
unsafeRelPathError fp
    = constructValidUnsafe
    . fromMaybe (error $ "Invalid path: " ++ fp)
    . relpath $ fp

abspath :: FilePath -> Maybe AbsPath
abspath [] = Nothing
abspath ['/'] = Just emptyPath
abspath ('/':fp) = unsafePathTypeCoerse <$> relpath fp
abspath _ = Nothing

unsafeAbsPathError :: FilePath -> AbsPath
unsafeAbsPathError fp
    = constructValidUnsafe
    . fromMaybe (error $ "Invalid path: " ++ fp)
    . abspath $ fp

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

ext :: String -> Maybe Extension
ext = constructValid . Extension . T.pack

unsafeExt :: String -> Extension
unsafeExt e
    = constructValidUnsafe
    . fromMaybe (error $ "Invalid extension: " ++ e)
    . ext $ e

-- | If the first path has extensions, they will be appended to the last
-- pathpiece before concatenation
(</>) :: Path rel -> RelPath -> Path rel
(</>) p1 p2
    | isEmptyPath p1 && isEmptyPath p2 = emptyPath
    | isEmptyPath p2 = p1
    | isEmptyPath p1 = unsafePathTypeCoerse p2
    | otherwise = Path
        { pathPieces = pathPieces p1 ++ [combineLastAndExtensions (pathLastPiece p1) (pathExtensions p1)] ++ pathPieces p2
        , pathLastPiece = pathLastPiece p2
        , pathExtensions = pathExtensions p2
        }

(<.>) :: Path rel -> Extension -> Path rel
(<.>) path extension
    | isEmptyPath path = path
    | otherwise = path
    { pathExtensions = pathExtensions path ++ [extension]
    }

ground :: AbsPath -> FilePath -> Maybe AbsPath
ground ap fp = case abspath fp of
    Just a -> Just a
    Nothing -> case relpath fp of
        Just r -> Just $ ap </> r
        Nothing -> Nothing

removeExtensions :: Path rel -> Path rel
removeExtensions (Path ps lp _) = Path ps lp []

takeLastPiece :: Path rel -> Text
takeLastPiece (Path _ (LastPathPiece t) _) = t
