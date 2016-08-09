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

pathSeparator :: Char
pathSeparator = '/'

extensionSeparator :: Char
extensionSeparator = '.'

containsSatisfied :: (Char -> Bool) -> Text -> Bool
containsSatisfied func = isJust . T.find func

containsSeparator :: Text -> Bool
containsSeparator = containsSatisfied (== pathSeparator)

containsDot :: Text -> Bool
containsDot = containsSatisfied (== extensionSeparator)

emptyLastPathPiece :: LastPathPiece -> Bool
emptyLastPathPiece (LastPathPiece "") = True
emptyLastPathPiece _ = False

emptyPath :: Path rel
emptyPath = (Path [] (LastPathPiece "") [])

isEmptyPath :: Path rel -> Bool
isEmptyPath p = p == emptyPath

relpath :: FilePath -> Maybe RelPath
relpath [] = Nothing
relpath fp@(c:rest)
    | c == extensionSeparator && null rest = Just emptyPath
    | c == pathSeparator = Nothing
    | otherwise = do
        let rawPieces = filter (not . T.null) $ T.split (== pathSeparator) $ T.pack fp
        (firstPieces, lastRawPiece) <- unsnocMay rawPieces
        let rawExts = filter (not . T.null) $ T.split (== extensionSeparator) lastRawPiece
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
abspath (c:fp)
  | c == pathSeparator && null fp = Just emptyPath
  | c == pathSeparator = unsafePathTypeCoerse <$> relpath fp
  | otherwise = Nothing

unsafeAbsPathError :: FilePath -> AbsPath
unsafeAbsPathError fp
    = constructValidUnsafe
    . fromMaybe (error $ "Invalid path: " ++ fp)
    . abspath $ fp

toRelFilePath :: RelPath -> FilePath
toRelFilePath (Path [] (LastPathPiece "") []) = [extensionSeparator]
toRelFilePath Path{..}
    =  intercalate [pathSeparator] (map renderPiece pathPieces ++ [renderLastPiece pathLastPiece])
    ++ renderExtensions pathExtensions

toAbsFilePath :: AbsPath -> FilePath
toAbsFilePath (Path [] (LastPathPiece "") []) = [pathSeparator]
toAbsFilePath p = (pathSeparator:) . toRelFilePath . unsafePathTypeCoerse $ p

renderPiece :: PathPiece -> String
renderPiece (PathPiece p) = T.unpack p

renderLastPiece :: LastPathPiece -> String
renderLastPiece (LastPathPiece p) = T.unpack p

renderExtension :: Extension -> String
renderExtension (Extension e) = T.unpack e

renderExtensions :: [Extension] -> String
renderExtensions [] = ""
renderExtensions es = [extensionSeparator] ++ intercalate [extensionSeparator] (map renderExtension es)

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
--
-- >>> "/directory/path" </> "another/path.ext" :: AbsPath
-- /directory/path/another/path.ext
-- >>> "directory/path"  </> "another/path.ext" :: RelPath
-- directory/path/another/path.ext
-- >>> "/file.ext1.ext2" </> "other/file.ext3"  :: AbsPath
-- /file.ext1.ext2/other/file.ext3
-- >>> "file.ext1.ext2"  </> "other/file.ext3"  :: RelPath
-- file.ext1.ext2/other/file.ext3
-- >>> "." </> "file.ext" :: RelPath
-- file.ext
-- >>> "/" </> "file.ext" :: AbsPath
-- /file.ext
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

-- | Add an extension to a path
--
-- >>> addExtension "/directory/path"       "ext"   :: AbsPath
-- /directory/path.ext
-- >>> addExtension "/directory/path.ext1"  "ext2"  :: AbsPath
-- /directory/path.ext1.ext2
-- >>> addExtension "directory/path"        "ext"   :: RelPath
-- directory/path.ext
-- >>> addExtension "directory/path.ext1"   "ext2"  :: RelPath
-- directory/path.ext1.ext2
-- >>> addExtension "." "ext" :: RelPath
-- .
-- >>> addExtension "/" "ext" :: AbsPath
-- /
addExtension :: Path rel -> Extension -> Path rel
addExtension path extension
    | isEmptyPath path = path
    | otherwise = path
      { pathExtensions = pathExtensions path ++ [extension] }

-- | Add an extension to a path (equivalent to 'addExtension')
--
-- >>> "/directory/path"      <.> "ext"   :: AbsPath
-- /directory/path.ext
-- >>> "/directory/path.ext1" <.> "ext2"  :: AbsPath
-- /directory/path.ext1.ext2
-- >>> "directory/path"       <.> "ext"   :: RelPath
-- directory/path.ext
-- >>> "directory/path.ext1"  <.> "ext2"  :: RelPath
-- directory/path.ext1.ext2
-- >>> "." <.> "ext" :: RelPath
-- .
-- >>> "/" <.> "ext" :: AbsPath
-- /
(<.>) :: Path rel -> Extension -> Path rel
(<.>) = addExtension

-- | Drop the last extension of a path
--
-- >>> dropExtension "dir/file.ext1.ext2" :: RelPath
-- dir/file.ext1
-- >>> dropExtension "dir/file.ext" :: RelPath
-- dir/file
-- >>> dropExtension "dir/file" :: RelPath
-- dir/file
-- >>> dropExtension "/dir/file.ext1.ext2" :: AbsPath
-- /dir/file.ext1
-- >>> dropExtension "/dir/file.ext" :: AbsPath
-- /dir/file
-- >>> dropExtension "/dir/file" :: AbsPath
-- /dir/file
-- >>> dropExtension "." :: RelPath
-- .
-- >>> dropExtension "/" :: AbsPath
-- /
dropExtension :: Path rel -> Path rel
dropExtension path = path
    { pathExtensions = reverse . drop 1 . reverse $ pathExtensions path }

-- | Drop all extensions of a path
--
-- >>> dropExtensions "dir/file.ext1.ext2" :: RelPath
-- dir/file
-- >>> dropExtensions "dir/file.ext" :: RelPath
-- dir/file
-- >>> dropExtensions "dir/file" :: RelPath
-- dir/file
-- >>> dropExtensions "/dir/file.ext1.ext2" :: AbsPath
-- /dir/file
-- >>> dropExtensions "/dir/file.ext" :: AbsPath
-- /dir/file
-- >>> dropExtensions "/dir/file" :: AbsPath
-- /dir/file
-- >>> dropExtensions "." :: RelPath
-- .
-- >>> dropExtensions "/" :: AbsPath
-- /
dropExtensions :: Path rel -> Path rel
dropExtensions (Path ps lp _) = Path ps lp []

-- | Replace the last extension of a path
--
-- >>> replaceExtension "dir/file.ext1.ext2"  "ext3" :: RelPath
-- dir/file.ext1.ext3
-- >>> replaceExtension "dir/file.ext1"       "ext2" :: RelPath
-- dir/file.ext2
-- >>> replaceExtension "dir/file"            "ext"  :: RelPath
-- dir/file.ext
-- >>> replaceExtension "/dir/file.ext1.ext2" "ext3" :: AbsPath
-- /dir/file.ext1.ext3
-- >>> replaceExtension "/dir/file.ext1"      "ext2" :: AbsPath
-- /dir/file.ext2
-- >>> replaceExtension "/dir/file"           "ext"  :: AbsPath
-- /dir/file.ext
-- >>> replaceExtension "." "ext" :: RelPath
-- .
-- >>> replaceExtension "/" "ext" :: AbsPath
-- /
replaceExtension :: Path rel -> Extension -> Path rel
replaceExtension path extension = dropExtension path <.> extension

-- | Replace the last extension of a path (equivalent to 'replaceExtension')
--
-- >>> "dir/file.ext1.ext2" -<.> "ext3"   :: RelPath
-- dir/file.ext1.ext3
-- >>> "dir/file.ext1" -<.> "ext2"        :: RelPath
-- dir/file.ext2
-- >>> "dir/file" -<.> "ext"              :: RelPath
-- dir/file.ext
-- >>> "/dir/file.ext1.ext2" -<.> "ext3"  :: AbsPath
-- /dir/file.ext1.ext3
-- >>> "/dir/file.ext1" -<.> "ext2"       :: AbsPath
-- /dir/file.ext2
-- >>> "/dir/file" -<.> "ext"             :: AbsPath
-- /dir/file.ext
-- >>> "." -<.> "ext" :: RelPath
-- .
-- >>> "/" -<.> "ext" :: AbsPath
-- /
(-<.>) :: Path rel -> Extension -> Path rel
(-<.>) = replaceExtension

ground :: AbsPath -> FilePath -> Maybe AbsPath
ground ap fp = case abspath fp of
    Just a -> Just a
    Nothing -> case relpath fp of
        Just r -> Just $ ap </> r
        Nothing -> Nothing

takeLastPiece :: Path rel -> Text
takeLastPiece (Path _ (LastPathPiece t) _) = t
