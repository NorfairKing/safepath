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
import Data.List (intercalate, foldl', stripPrefix)
import Data.Data

import Data.Text (Text)
import qualified Data.Text as T

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
-- This instance instance is unsafe and should only be used at own risk,
-- for literals
instance IsString (Path Absolute) where
    fromString = unsafeAbsPathError

-- | ONLY for @OverloadedStrings@
-- This instance instance is unsafe and should only be used at own risk,
-- for literals
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
    deriving (Eq, Generic, Data, Typeable)

instance Show PathPiece where
    show (PathPiece t) = T.unpack t

instance Validity PathPiece where
    isValid (PathPiece t) = not (T.null t) && not (containsSeparator t)

-- | ONLY for @OverloadedStrings@
-- This instance instance is unsafe and should only be used at own risk,
-- for literals
instance IsString PathPiece where
    fromString = unsafePathPieceError

newtype LastPathPiece = LastPathPiece Text
    deriving (Eq, Generic, Data, Typeable)

instance Show LastPathPiece where
    show (LastPathPiece t) = T.unpack t

instance Validity LastPathPiece where
    isValid (LastPathPiece t) = not (containsSeparator t) && not (containsExtension t)

-- | ONLY for @OverloadedStrings@
-- This instance instance is unsafe and should only be used at own risk,
-- for literals
instance IsString LastPathPiece where
    fromString = unsafeLastPieceError

newtype Extension = Extension Text
    deriving (Eq, Generic, Data, Typeable)

instance Show Extension where
    show (Extension t) = T.unpack t

-- | ONLY for @OverloadedStrings@
-- This instance instance is unsafe and should only be used at own risk,
-- for literals
instance IsString Extension where
    fromString = unsafeExtError

instance Validity Extension where
    isValid (Extension t) = not (T.null t) && not (containsExtension t) && not (containsSeparator t)

pathSeparator :: Char
pathSeparator = '/'

pathSeparators :: [Char]
pathSeparators = [pathSeparator]

-- | Check if a given character is a valid path separator
--
-- >>> isPathSeparator pathSeparator
-- True
isPathSeparator :: Char -> Bool
isPathSeparator = (== pathSeparator)

extensionSeparator :: Char
extensionSeparator = '.'

extensionSeparators :: [Char]
extensionSeparators = [extensionSeparator]

-- | Check if a given character is a valid extension separator
--
-- >>> isExtensionSeparator extensionSeparator
-- True
isExtensionSeparator :: Char -> Bool
isExtensionSeparator = (== extensionSeparator)

containsSatisfied :: (Char -> Bool) -> Text -> Bool
containsSatisfied func = isJust . T.find func

containsSeparator :: Text -> Bool
containsSeparator = containsSatisfied isPathSeparator

containsExtension :: Text -> Bool
containsExtension = containsSatisfied isExtensionSeparator

-- | Construct a relative path from a 'FilePath', failing if
-- the given 'FilePath' does not represent a valid relative path.
--
-- >>> relpath "file"
-- Just file
-- >>> relpath "/file"
-- Nothing
-- >>> relpath "."
-- Just .
-- >>> relpath "/"
-- Nothing
-- >>> relpath ""
-- Nothing
relpath :: FilePath -> Maybe RelPath
relpath [] = Nothing
relpath fp@(c:rest)
    | c == extensionSeparator && null rest = Just emptyPath
    | c == pathSeparator = Nothing
    | last fp == extensionSeparator = Nothing
    | otherwise = do
        let rawPieces = filter (not . T.null) $ T.split (== pathSeparator) $ T.pack fp
        (pieces, lastRawPiece) <- unsnoc $ map PathPiece rawPieces
        let (lastPiece, exts) = splitPiece lastRawPiece
        return $ Path pieces lastPiece exts


-- | Construct an absolute path from a 'FilePath', failing if
-- the given 'FilePath' does not represent a valid absolute path.
--
-- >>> abspath "/file"
-- Just /file
-- >>> abspath "file"
-- Nothing
-- >>> abspath "/"
-- Just /
-- >>> abspath "."
-- Nothing
-- >>> abspath ""
-- Nothing
abspath :: FilePath -> Maybe AbsPath
abspath [] = Nothing
abspath (c:fp)
  | c == pathSeparator && null fp = Just emptyPath
  | c == pathSeparator = unsafePathTypeCoerse <$> relpath fp
  | otherwise = Nothing

-- | Construct a path piece safely
--
-- >>> pathpiece "file"
-- Just file
-- >>> pathpiece "with.dot"
-- Just with.dot
-- >>> pathpiece "with/slash"
-- Nothing
pathpiece :: String -> Maybe PathPiece
pathpiece = constructValid . PathPiece . T.pack

-- | Construct a last path piece safely
--
-- >>> lastpiece "file"
-- Just file
-- >>> lastpiece "with.dot"
-- Nothing
lastpiece :: String -> Maybe LastPathPiece
lastpiece = constructValid . LastPathPiece . T.pack

-- | Construct an extension safely
--
-- >>> ext "extension"
-- Just extension
-- >>> ext ".ext"
-- Nothing
-- >>> ext ""
-- Nothing
ext :: String -> Maybe Extension
ext = constructValid . Extension . T.pack

-- | Ground a filepath on an absolute path.
-- This will try to parse the given @FilePath@ as an absolute path and take it
-- if that works. Otherwise it will try to parse it an a relative path and
-- append it to the given @AbsPath@
--
-- >>> ground "/home/user" "relative/path"
-- Just /home/user/relative/path
-- >>> ground "/home/user" "/absolute/path"
-- Just /absolute/path
-- >>> ground "/home/user" "."
-- Just /home/user
-- >>> ground "/home/user" "/"
-- Just /
-- >>> ground "/" "."
-- Just /
-- >>> ground "/anything" ""
-- Nothing
ground :: AbsPath -> FilePath -> Maybe AbsPath
ground ap fp = case abspath fp of
    Just a -> Just a
    Nothing -> case relpath fp of
        Just r -> Just $ ap </> r
        Nothing -> Nothing

-- | Construct a relative path, throwing an 'error'
-- if 'relpath' would fail.
unsafeRelPathError :: FilePath -> RelPath
unsafeRelPathError fp
    = constructValidUnsafe
    . fromMaybe (error $ "Invalid path: " ++ fp)
    . relpath $ fp


-- | Construct an absolute path, throwing an 'error'
-- if 'abspath' would fail.
unsafeAbsPathError :: FilePath -> AbsPath
unsafeAbsPathError fp
    = constructValidUnsafe
    . fromMaybe (error $ "Invalid path: " ++ fp)
    . abspath $ fp

unsafePathPieceError :: String -> PathPiece
unsafePathPieceError s
    = constructValidUnsafe
    . fromMaybe (error $ "Invalid path piece: " ++ s)
    . pathpiece $ s

unsafeLastPieceError :: String -> LastPathPiece
unsafeLastPieceError s
    = constructValidUnsafe
    . fromMaybe (error $ "Invalid last path piece: " ++ s)
    . lastpiece $ s

-- | Construct an extension, throwing an 'error' if 'ext' would fail.
unsafeExtError :: String -> Extension
unsafeExtError e
    = constructValidUnsafe
    . fromMaybe (error $ "Invalid extension: " ++ e)
    . ext $ e

-- | Render a relative filepath to a 'FilePath'
toRelFilePath :: RelPath -> FilePath
toRelFilePath (Path [] (LastPathPiece "") []) = [extensionSeparator]
toRelFilePath Path{..}
    =  intercalate [pathSeparator] (map renderPiece pathPieces ++ [renderLastPiece pathLastPiece])
    ++ renderExtensions pathExtensions

-- | Render an absolute filepath to a 'FilePath'
toAbsFilePath :: AbsPath -> FilePath
toAbsFilePath (Path [] (LastPathPiece "") []) = [pathSeparator]
toAbsFilePath p = (pathSeparator:) . toRelFilePath . unsafePathTypeCoerse $ p

-- | Take the last extension of a filepath
--
-- >>> takeExtension ("/directory/path.ext" :: AbsPath)
-- Just ext
-- >>> takeExtension ("file.tar.gz" :: RelPath)
-- Just gz
-- >>> takeExtension ("file" :: RelPath)
-- Nothing
--
-- Replaces @System.FilePath.takeExtension@
takeExtension :: Path rel -> Maybe Extension
takeExtension (Path _ _ es) = lastMay es

-- | Take all extensions of a given path in the form of a list
--
-- >>> takeExtensions ("/directory/path.ext" :: AbsPath)
-- [ext]
-- >>> takeExtensions ("file.tar.gz" :: RelPath)
-- [tar,gz]
--
-- Replaces @System.FilePath.takeExtensions@
takeExtensions :: Path rel -> [Extension]
takeExtensions (Path _ _ es) = es

-- | Replace the last extension of a path
--
-- This will first remove one extension and then add the given extension.
--
-- > replaceExtension path extension = dropExtension path <.> extension
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
--
-- Replaces @System.FilePath.replaceExtension@

-- TODO(syd) exact version
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
--
-- Replaces @System.FilePath.(-<.>)@
(-<.>) :: Path rel -> Extension -> Path rel
(-<.>) = replaceExtension

-- | Replace all the extensions of a path with the given extension
--
-- >>> replaceExtensions "dir/file.ext1.ext2"  "ext3" :: RelPath
-- dir/file.ext3
-- >>> replaceExtensions "dir/file.ext1"       "ext3" :: RelPath
-- dir/file.ext3
-- >>> replaceExtensions "dir/file"            "ext3" :: RelPath
-- dir/file.ext3
-- >>> replaceExtensions "/dir/file.ext1.ext2" "ext3" :: AbsPath
-- /dir/file.ext3
-- >>> replaceExtensions "/dir/file.ext1"      "ext3" :: AbsPath
-- /dir/file.ext3
-- >>> replaceExtensions "/dir/file"           "ext3" :: AbsPath
-- /dir/file.ext3
-- >>> replaceExtensions "." "ext" :: RelPath
-- .
-- >>> replaceExtensions "/" "ext" :: AbsPath
-- /

-- TODO(syd) exact version
replaceExtensions :: Path rel -> Extension -> Path rel
replaceExtensions p e = replaceExtensionss p [e]

-- | Replace all the extensions of a path with the given list of extensions
--
-- >>> replaceExtensionss "dir/file.ext1.ext2"  ["ext3", "ext4"] :: RelPath
-- dir/file.ext3.ext4
-- >>> replaceExtensionss "dir/file.ext1"       ["ext3", "ext4"] :: RelPath
-- dir/file.ext3.ext4
-- >>> replaceExtensionss "dir/file"            ["ext3", "ext4"] :: RelPath
-- dir/file.ext3.ext4
-- >>> replaceExtensionss "/dir/file.ext1.ext2" ["ext3", "ext4"] :: AbsPath
-- /dir/file.ext3.ext4
-- >>> replaceExtensionss "/dir/file.ext1"      ["ext3", "ext4"] :: AbsPath
-- /dir/file.ext3.ext4
-- >>> replaceExtensionss "/dir/file"           ["ext3", "ext4"] :: AbsPath
-- /dir/file.ext3.ext4
-- >>> replaceExtensionss "." ["ext1", "ext2"] :: RelPath
-- .
-- >>> replaceExtensionss "/" ["ext1", "ext2"] :: AbsPath
-- /
replaceExtensionss :: Path rel -> [Extension] -> Path rel
replaceExtensionss p@(Path ps lp _) es
  | isEmptyPath p = emptyPath
  | otherwise = (Path ps lp es)

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
--
-- Replaces @System.FilePath.dropExtension@

-- TODO(syd) exact version
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
--
-- Replaces @System.FilePath.dropExtensions@

-- TODO(syd) exact version
dropExtensions :: Path rel -> Path rel
dropExtensions (Path ps lp _) = Path ps lp []

-- | Add an extension to a path
--
-- >>> addExtension "/directory/path" "ext" :: AbsPath
-- /directory/path.ext
-- >>> addExtension "directory/path"  "ext" :: RelPath
-- directory/path.ext
--
-- This will not override the extension if there already is an extension.
-- It will only add the given extension on top of it
--
-- >>> addExtension "/directory/path.ext1" "ext2" :: AbsPath
-- /directory/path.ext1.ext2
-- >>> addExtension "directory/path.ext1"  "ext2" :: RelPath
-- directory/path.ext1.ext2
--
-- This will not add an extension if the path is empty.
--
-- >>> addExtension "." "ext" :: RelPath
-- .
-- >>> addExtension "/" "ext" :: AbsPath
-- /
--
-- Replaces @System.FilePath.addExtension@
addExtension :: Path rel -> Extension -> Path rel
addExtension path extension
    | isEmptyPath path = path
    | otherwise = path
      { pathExtensions = pathExtensions path ++ [extension] }

-- | Add an extension to a path (equivalent to 'addExtension')
--
-- >>> "/directory/path" <.> "ext" :: AbsPath
-- /directory/path.ext
-- >>> "directory/path"  <.> "ext" :: RelPath
-- directory/path.ext
-- >>> "/directory/path.ext1" <.> "ext2" :: AbsPath
-- /directory/path.ext1.ext2
-- >>> "directory/path.ext1"  <.> "ext2" :: RelPath
-- directory/path.ext1.ext2
-- >>> "." <.> "ext" :: RelPath
-- .
-- >>> "/" <.> "ext" :: AbsPath
-- /
--
-- Replaces @System.FilePath.(<.>)@
(<.>) :: Path rel -> Extension -> Path rel
(<.>) = addExtension

-- | Add a list of extensions to a path
--
-- >>> addExtensions "/directory/path" ["ext1", "ext2"] :: AbsPath
-- /directory/path.ext1.ext2
-- >>> addExtensions "directory/path"  ["ext1", "ext2"] :: RelPath
-- directory/path.ext1.ext2
--
-- >>> addExtensions "/directory/path.ext1" ["ext2", "ext3"] :: AbsPath
-- /directory/path.ext1.ext2.ext3
-- >>> addExtensions "directory/path.ext1"  ["ext2", "ext3"] :: RelPath
-- directory/path.ext1.ext2.ext3
--
-- >>> addExtensions "." ["ext1", "ext2"] :: RelPath
-- .
-- >>> addExtensions "/" ["ext1", "ext2"] :: AbsPath
-- /
--
-- This operation is an identity function if the given list of extensions
-- is empty.
addExtensions :: Path rel -> [Extension] -> Path rel
addExtensions = foldl' addExtension


-- | Drop the given extension from a FilePath.
-- Fails if the FilePath does not have the given extension.
--
-- >>> stripExtension "foo.x.hs.o" "o"    :: Maybe RelPath
-- Just foo.x.hs
-- >>> stripExtension "foo.x.hs.o" "hs"   :: Maybe RelPath
-- Nothing
-- >>> stripExtension "a.b.c.d"    "d"    :: Maybe RelPath
-- Just a.b.c
-- >>> stripExtension "foo.bar"    "baz"  :: Maybe RelPath
-- Nothing
-- >>> stripExtension "foobar"     "bar"  :: Maybe RelPath
-- Nothing
--
-- Replaces @System.FilePath.stripExtension@
stripExtension :: Path rel -> Extension -> Maybe (Path rel)
stripExtension p e = stripExtensions p [e]

-- | Drop the given extensions from a FilePath.
-- Fails if the FilePath does not have all of the given extensions.
--
-- >>> stripExtensions "foo.x.hs.o" ["hs", "o"]      :: Maybe RelPath
-- Just foo.x
-- >>> stripExtensions "foo.x.hs.o" ["o", "hs"]      :: Maybe RelPath
-- Nothing
-- >>> stripExtensions "a.b.c.d"    ["c", "d"]       :: Maybe RelPath
-- Just a.b
-- >>> stripExtensions "foo.bar"    ["baz", "quux"]  :: Maybe RelPath
-- Nothing
-- >>> stripExtensions "foobar"     ["bar"]          :: Maybe RelPath
-- Nothing
stripExtensions :: Path rel -> [Extension] -> Maybe (Path rel)
stripExtensions (Path ps lp es) esq
    = (Path ps lp . reverse) <$> stripPrefix (reverse esq) (reverse es)

-- | Split off the extensions from a path
--
-- >>> splitExtension ("dir/file.ext1.ext2" :: RelPath)
-- Just (dir/file.ext1,ext2)
-- >>> splitExtension ("dir/file.ext" :: RelPath)
-- Just (dir/file,ext)
-- >>> splitExtension ("dir/file" :: RelPath)
-- Nothing
-- >>> splitExtension ("/dir/file.ext1.ext2" :: AbsPath)
-- Just (/dir/file.ext1,ext2)
-- >>> splitExtension ("/dir/file.ext" :: AbsPath)
-- Just (/dir/file,ext)
-- >>> splitExtension ("/dir/file" :: AbsPath)
-- Nothing
-- >>> splitExtension ("." :: RelPath)
-- Nothing
-- >>> splitExtension ("/" :: AbsPath)
-- Nothing
--
-- Replaces @System.FilePath.splitExtension@
splitExtension :: Path rel -> Maybe (Path rel, Extension)
splitExtension p = (,) (dropExtension p) <$> takeExtension p

-- | Split off the extensions from a path
--
-- >>> splitExtensions ("dir/file.ext1.ext2" :: RelPath)
-- (dir/file,[ext1,ext2])
-- >>> splitExtensions ("dir/file.ext" :: RelPath)
-- (dir/file,[ext])
-- >>> splitExtensions ("dir/file" :: RelPath)
-- (dir/file,[])
-- >>> splitExtensions ("/dir/file.ext1.ext2" :: AbsPath)
-- (/dir/file,[ext1,ext2])
-- >>> splitExtensions ("/dir/file.ext" :: AbsPath)
-- (/dir/file,[ext])
-- >>> splitExtensions ("/dir/file" :: AbsPath)
-- (/dir/file,[])
-- >>> splitExtensions ("." :: RelPath)
-- (.,[])
-- >>> splitExtensions ("/" :: AbsPath)
-- (/,[])
splitExtensions :: Path rel -> (Path rel, [Extension])
splitExtensions p = (dropExtensions p, takeExtensions p)

-- | Check whether the given filepath has any extensions
--
-- >>> hasExtension ("/directory/path.ext" :: AbsPath)
-- True
-- >>> hasExtension ("/directory/path"     :: AbsPath)
-- False
--
-- Replaces @System.FilePath.hasExtension@
hasExtension :: Path rel -> Bool
hasExtension = not . null . takeExtensions

-- | Split a path into all but the last piece and the last piece and the
-- extensions
--
-- >>> splitFileName ("/directory/file.ext" :: AbsPath)
-- (/directory,file.ext)
-- >>> splitFileName ("file/bob.txt" :: RelPath)
-- (file,bob.txt)
-- >>> splitFileName ("file" :: RelPath)
-- (.,file)
-- >>> splitFileName ("dir.ext/file.ext" :: RelPath)
-- (dir.ext,file.ext)
splitFileName :: Path rel -> (Path rel, RelPath)
splitFileName (Path ps lp es)
    = case unsnoc ps of
          Nothing -> (emptyPath, Path [] lp es)
          Just (firsts, lastp) ->
              let (lp', es') = splitPiece lastp
              in (Path firsts lp' es', Path [] lp es)

-- | Take the last piece and the extensions, exactly.
--
-- This will evaluate to 'Nothing' if the given path is empty
--
-- >>> takeFileNameExact ("/directory/file.ext" :: AbsPath)
-- Just file.ext
-- >>> takeFileNameExact ("file/bob.txt" :: RelPath)
-- Just bob.txt
-- >>> takeFileNameExact ("file" :: RelPath)
-- Just file
-- >>> takeFileNameExact ("dir.ext/file.ext" :: RelPath)
-- Just file.ext
-- >>> takeFileNameExact ("." :: RelPath)
-- Nothing
-- >>> takeFileNameExact ("/" :: AbsPath)
-- Nothing
--
-- Replaces @System.FilePath.takeFileName@
takeFileNameExact :: Path rel -> Maybe RelPath
takeFileNameExact p@(Path _ lp es)
    | isEmptyPath p = Nothing
    | otherwise = Just $ Path [] lp es

-- | Take the last piece and the extensions.
--
-- This will evaluate to the empty (relative) path if the given path is empty.
--
-- >>> takeFileName ("/directory/file.ext" :: AbsPath)
-- file.ext
-- >>> takeFileName ("file/bob.txt" :: RelPath)
-- bob.txt
-- >>> takeFileName ("file" :: RelPath)
-- file
-- >>> takeFileName ("dir.ext/file.ext" :: RelPath)
-- file.ext
-- >>> takeFileName ("." :: RelPath)
-- .
-- >>> takeFileName ("/" :: AbsPath)
-- .
--
-- Replaces @System.FilePath.takeFileName@
takeFileName :: Path rel -> RelPath
takeFileName p
    = case takeFileNameExact p of
          Nothing -> emptyPath
          Just r  -> r

-- | Replace the last piece of a path with the given last piece.
--
-- >>> replaceFileNameExact "/directory/other.txt" "file.ext" :: Maybe AbsPath
-- Just /directory/file.ext
-- >>> replaceFileNameExact "." "file.ext" :: Maybe RelPath
-- Just file.ext
-- >>> replaceFileNameExact "/" "file.ext" :: Maybe AbsPath
-- Just /file.ext
--
-- If the given path piece is degenerate, this is what happens:
--
-- >>> replaceFileNameExact "/directory/other.txt" "..." :: Maybe AbsPath
-- Nothing
replaceFileNameExact :: Path rel -> PathPiece -> Maybe (Path rel)
replaceFileNameExact (Path ps _ _) p
    = let (lp, es) = splitPiece p
      in if isEmptyLastPathPiece lp
         && (not (null es) || not (null ps))
          then Nothing
          else Just $ Path ps lp es

-- | Replace the last piece of a path with the given last piece.
--
-- >>> replaceFileName "/directory/other.txt" "file.ext" :: AbsPath
-- /directory/file.ext
-- >>> replaceFileName "." "file.ext" :: RelPath
-- file.ext
-- >>> replaceFileName "/" "file.ext" :: AbsPath
-- /file.ext
--
-- If the given path piece is degenerate, this is what happens:
--
-- >>> replaceFileName "/directory/other.txt" "..." :: AbsPath
-- /directory
replaceFileName :: Path rel -> PathPiece -> Path rel
replaceFileName path p
    = case replaceFileNameExact path p of
        Nothing -> dropFileName path
        Just rs -> rs

-- | Drop the last piece of a path, exactly
--
-- >>> dropFileNameExact ("directory/file.ext" :: RelPath)
-- Just directory
-- >>> dropFileNameExact ("/directory/file.ext" :: AbsPath)
-- Just /directory
--
-- This evaluates to Nothing when given an empty path
--
-- >>> dropFileNameExact ("/" :: AbsPath)
-- Nothing
-- >>> dropFileNameExact ("." :: RelPath)
-- Nothing
dropFileNameExact :: Path rel -> Maybe (Path rel)
dropFileNameExact (Path psc _ _)
    = case unsnoc psc of
        Nothing -> Nothing
        Just (ps, p) ->
            let (lp, es) = splitPiece p
            in if isEmptyLastPathPiece lp
               then Nothing -- TODO(syd) fixme: really ugly
               else Just $ Path ps lp es

-- | Drop the last piece of a path
--
-- >>> dropFileName ("directory/file.ext" :: RelPath)
-- directory
-- >>> dropFileName ("/directory/file.ext" :: AbsPath)
-- /directory
--
-- This evaluates to an empty path when given an empty path
--
-- >>> dropFileName ("/" :: AbsPath)
-- /
-- >>> dropFileName ("." :: RelPath)
-- .
--
-- Replaces @System.FilePath.dropFileName@
dropFileName :: Path rel -> Path rel
dropFileName p
    = case dropFileNameExact p of
        Nothing -> emptyPath
        Just rs -> rs

-- | Take the last piece (no extensions)
--
-- >>> takeBaseNameExact ("file.ext" :: RelPath)
-- Just file
-- >>> takeBaseNameExact ("dir/and/file.ext" :: RelPath)
-- Just file
--
-- This will evaluate to Nothing when given an empty path:
--
-- >>> takeBaseNameExact ("." :: RelPath)
-- Nothing
-- >>> takeBaseNameExact ("/" :: AbsPath)
-- Nothing
takeBaseNameExact :: Path rel -> Maybe LastPathPiece
takeBaseNameExact p@(Path _ lp _)
    | isEmptyPath p = Nothing
    | otherwise = Just lp

-- | Take the last piece (no extensions)
--
-- >>> takeBaseName ("file.ext" :: RelPath)
-- file
-- >>> takeBaseName ("dir/and/file.ext" :: RelPath)
-- file
--
-- This will evaluate to an empty last path piece when given an empty path:
--
-- Replaces @System.FilePath.takeBaseName@
takeBaseName :: Path rel -> LastPathPiece
takeBaseName p
    = case takeBaseNameExact p of
        Nothing -> emptyLastPathPiece
        Just rs -> rs

-- | Replace the last piece exactly: fails on empty last piece
--
-- >>> replaceBaseNameExact "file.ext" "piece" :: Maybe RelPath
-- Just piece.ext
-- >>> replaceBaseNameExact "." "thing" :: Maybe RelPath
-- Just thing
-- >>> replaceBaseNameExact "/" "thing" :: Maybe AbsPath
-- Just /thing
-- >>> replaceBaseNameExact "/directory/file" "" :: Maybe AbsPath
-- Nothing
replaceBaseNameExact :: Path rel -> LastPathPiece -> Maybe (Path rel)
replaceBaseNameExact (Path ps _ es) lp
    | isEmptyLastPathPiece lp = Nothing
    | otherwise = Just $ Path ps lp es

-- | Replace the last piece
--
-- >>> replaceBaseName "file.ext" "piece" :: RelPath
-- piece.ext
-- >>> replaceBaseName "." "thing" :: RelPath
-- thing
-- >>> replaceBaseName "/" "thing" :: AbsPath
-- /thing
-- >>> replaceBaseName "/directory/file" "" :: AbsPath
-- /directory
--
-- Replaces @System.FilePath.replaceBaseName@
replaceBaseName :: Path rel -> LastPathPiece -> Path rel
replaceBaseName p@(Path ps _ es) lp
    | isEmptyLastPathPiece lp = dropFileName p
    | otherwise = Path ps lp es

-- | Replace everthing but the last piece, exactly
--
-- >>> replaceDirectoryExact ("/dir/and/file" :: AbsPath) ("other/directory" :: RelPath)
-- Just other/directory/file
--
-- This will evaluate to 'Nothing' if the first argument is an empty path.
--
-- >>> replaceDirectoryExact ("." :: RelPath) ("a/directory" :: RelPath)
-- Nothing
-- >>> replaceDirectoryExact ("/" :: AbsPath) ("a/directory" :: RelPath)
-- Nothing
--
--
-- This will evaluate to 'Nothing' if the second argument is an empty path.
--
-- >>> replaceDirectoryExact ("dir/file" :: RelPath) ("." :: RelPath)
-- Nothing
-- >>> replaceDirectoryExact ("dir/file" :: RelPath) ("/" :: AbsPath)
-- Nothing
replaceDirectoryExact :: Path r -> Path s -> Maybe (Path s)
replaceDirectoryExact p@(Path _ lp es) q@(Path ps' lp' es')
    | isEmptyPath p = Nothing
    | isEmptyPath q = Nothing
    | otherwise =
        let p = combineLastAndExtensions lp' es'
        in Just $ Path (ps' ++ [p]) lp es

-- | Replace everthing but the last piece
--
-- >>> replaceDirectory ("/dir/and/file" :: AbsPath) ("other/directory" :: RelPath)
-- other/directory/file
-- >>> replaceDirectory ("." :: RelPath) ("a/directory" :: RelPath)
-- a/directory
-- >>> replaceDirectory ("/" :: AbsPath) ("a/directory" :: RelPath)
-- a/directory
-- >>> replaceDirectory ("dir/file" :: RelPath) ("." :: RelPath)
-- file
-- >>> replaceDirectory ("dir/file" :: RelPath) ("/" :: AbsPath)
-- /file
--
-- Replaces @System.FilePath.replaceDirectory@
replaceDirectory :: Path r -> Path s -> Path s
replaceDirectory p@(Path _ lp es) q@(Path ps' lp' es')
    | isEmptyPath p = q
    | isEmptyPath q = Path [] lp es
    | otherwise =
        let p = combineLastAndExtensions lp' es'
        in Path (ps' ++ [p]) lp es

-- | Combine two paths, exactly
--
-- If the first path has extensions, they will be appended to the last
-- pathpiece before concatenation
--
-- >>> combineExact "/directory/path" "another/path.ext" :: Maybe AbsPath
-- Just /directory/path/another/path.ext
-- >>> combineExact "directory/path"  "another/path.ext" :: Maybe RelPath
-- Just directory/path/another/path.ext
-- >>> combineExact "/file.ext1.ext2" "other/file.ext3"  :: Maybe AbsPath
-- Just /file.ext1.ext2/other/file.ext3
-- >>> combineExact "file.ext1.ext2"  "other/file.ext3"  :: Maybe RelPath
-- Just file.ext1.ext2/other/file.ext3
--
-- This evaluates to 'Nothing' if any of the given paths are empty
--
-- >>> combineExact "." "file.ext" :: Maybe RelPath
-- Nothing
-- >>> combineExact "/" "file.ext" :: Maybe AbsPath
-- Nothing
combineExact :: Path rel -> RelPath -> Maybe (Path rel)
combineExact p1 p2
    | isEmptyPath p1 || isEmptyPath p2 = Nothing
    | otherwise = Just Path
        { pathPieces =
            pathPieces p1
            ++
            [combineLastAndExtensions (pathLastPiece p1) (pathExtensions p1)]
            ++
            pathPieces p2
        , pathLastPiece = pathLastPiece p2
        , pathExtensions = pathExtensions p2
        }

-- | Combine two paths
--
-- If the first path has extensions, they will be appended to the last
-- pathpiece before concatenation
--
-- >>> combine "/directory/path" "another/path.ext" :: AbsPath
-- /directory/path/another/path.ext
-- >>> combine "directory/path"  "another/path.ext" :: RelPath
-- directory/path/another/path.ext
-- >>> combine "/file.ext1.ext2" "other/file.ext3"  :: AbsPath
-- /file.ext1.ext2/other/file.ext3
-- >>> combine "file.ext1.ext2"  "other/file.ext3"  :: RelPath
-- file.ext1.ext2/other/file.ext3
--
-- This treats empty paths as identities to the operation.
--
-- >>> combine "file.ext" "." :: RelPath
-- file.ext
-- >>> combine "." "file.ext" :: RelPath
-- file.ext
-- >>> combine "/" "file.ext" :: AbsPath
-- /file.ext
-- >>> combine "." "." :: RelPath
-- .
-- >>> combine "/" "." :: AbsPath
-- /
--
-- Replaces @System.FilePath.combine@
combine :: Path rel -> RelPath -> Path rel
combine p1 p2
    | isEmptyPath p1 && isEmptyPath p2 = emptyPath
    | isEmptyPath p2 = p1
    | isEmptyPath p1 = unsafePathTypeCoerse p2
    | otherwise = Path
        { pathPieces =
            pathPieces p1
            ++
            [combineLastAndExtensions (pathLastPiece p1) (pathExtensions p1)]
            ++
            pathPieces p2
        , pathLastPiece = pathLastPiece p2
        , pathExtensions = pathExtensions p2
        }

-- | Combine two paths
--
-- equivalent to 'combine'
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
--
-- Replaces @System.FilePath.(</>)@
(</>) :: Path rel -> RelPath -> Path rel
(</>) = combine

-- | Split a path up into pieces
--
-- >>> splitPath ("/a/full/absolute/directory/path" :: AbsPath)
-- [a,full,absolute,directory,path]
splitPath :: Path rel -> [PathPiece]
splitPath (Path ps lp es)
    = ps ++ filter isValid [combineLastAndExtensions lp es]

-- | Join path pieces back into a path
--
-- >>> joinPath ["a", "full", "absolute", "directory", "path"] :: Maybe AbsPath
-- Just /a/full/absolute/directory/path
-- >>> joinPath [] :: Maybe RelPath
-- Just .
-- >>> joinPath [] :: Maybe AbsPath
-- Just /
-- >>> joinPath [".", "."] :: Maybe RelPath
-- Nothing
joinPath :: [PathPiece] -> Maybe (Path rel)
joinPath ps =
    case unsnoc ps of
        Nothing -> Just $ emptyPath
        Just (ips, p) ->
            let (lp, es) = splitPiece p
            in constructValid $ Path ips lp es


--- [ UTILS ] ---

initMay :: [a] -> Maybe [a]
initMay [] = Nothing
initMay as = Just $ reverse $ tail $ reverse as

lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay as = Just $ head $ reverse as

unsnoc :: [a] -> Maybe ([a], a)
unsnoc as = (,) <$> initMay as <*> lastMay as

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (a:as) = Just (a, as)

isEmptyLastPathPiece :: LastPathPiece -> Bool
isEmptyLastPathPiece = (== emptyLastPathPiece)

emptyLastPathPiece :: LastPathPiece
emptyLastPathPiece = (LastPathPiece T.empty)

emptyPath :: Path rel
emptyPath = (Path [] emptyLastPathPiece [])

isEmptyPath :: Path rel -> Bool
isEmptyPath p = p == emptyPath

renderPiece :: PathPiece -> String
renderPiece (PathPiece p) = T.unpack p

renderLastPiece :: LastPathPiece -> String
renderLastPiece (LastPathPiece p) = T.unpack p

renderExtension :: Extension -> String
renderExtension (Extension e) = T.unpack e

renderExtensions :: [Extension] -> String
renderExtensions [] = []
renderExtensions es = [extensionSeparator]
    ++ intercalate [extensionSeparator] (map renderExtension es)

combineLastAndExtensions :: LastPathPiece -> [Extension] -> PathPiece
combineLastAndExtensions (LastPathPiece lpp) es
    = PathPiece $ lpp <> T.pack (renderExtensions es)

splitPiece :: PathPiece -> (LastPathPiece, [Extension])
splitPiece (PathPiece t) =
    let rawExts = filter (not . T.null) $ T.split (== extensionSeparator) t
    in case uncons rawExts of
        Nothing -> (emptyLastPathPiece, [])
        Just (lastPieceStr, safeExts) ->
            let lastPiece = LastPathPiece lastPieceStr
                exts = map Extension safeExts
            in (lastPiece, exts)

unsafePathTypeCoerse :: Path rel -> Path rel'
unsafePathTypeCoerse (Path pieces lastPiece exts) = Path pieces lastPiece exts

