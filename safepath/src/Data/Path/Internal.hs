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
import Safe

import Data.Validity

unsnoc :: [a] -> Maybe ([a], a)
unsnoc as = (,) <$> initMay as <*> lastMay as

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (a:as) = Just (a, as)

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
    isValid (LastPathPiece t) = not (containsSeparator t) && not (containsDot t)

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

splitPiece :: PathPiece -> (LastPathPiece, [Extension])
splitPiece (PathPiece t) =
    let rawExts = filter (not . T.null) $ T.split (== extensionSeparator) t
        lastPieceStr = head rawExts
        safeExts = tail rawExts
        lastPiece = LastPathPiece lastPieceStr
        exts = map Extension safeExts
    in (lastPiece, exts)

unsafePathTypeCoerse :: Path rel -> Path rel'
unsafePathTypeCoerse (Path pieces lastPiece exts) = Path pieces lastPiece exts

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
(<.>) :: Path rel -> Extension -> Path rel
(<.>) = addExtension

-- | Check whether the given filepath has any extensions
--
-- >>> hasExtension ("/directory/path.ext" :: AbsPath)
-- True
-- >>> hasExtension ("/directory/path"     :: AbsPath)
-- False
hasExtension :: Path rel -> Bool
hasExtension = not . null . takeExtensions

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

-- | Take all extensions of a given path in the form of a list
--
-- >>> takeExtensions ("/directory/path.ext" :: AbsPath)
-- [ext]
-- >>> takeExtensions ("file.tar.gz" :: RelPath)
-- [tar,gz]
takeExtensions :: Path rel -> [Extension]
takeExtensions (Path _ _ es) = es

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

-- | Take the last piece and the extensions.
--
-- >>> takeFileName ("/directory/file.ext" :: AbsPath)
-- file.ext
-- >>> takeFileName ("file/bob.txt" :: RelPath)
-- bob.txt
-- >>> takeFileName ("file" :: RelPath)
-- file
-- >>> takeFileName ("dir.ext/file.ext" :: RelPath)
-- file.ext
takeFileName :: Path rel -> RelPath
takeFileName (Path _ lp es) = Path [] lp es

-- | Replace the last piece of a path with the given last piece.
--
-- >>> replaceFileName "/directory/other.txt" "file.ext" :: AbsPath
-- /directory/file.ext
replaceFileName :: Path rel -> PathPiece -> Path rel
replaceFileName (Path ps _ _) p
    = let (lp, es) = splitPiece p
      in Path ps lp es

dropFileName :: Path rel -> Path rel
dropFileName = undefined

takeBaseName :: Path rel -> LastPathPiece
takeBaseName = undefined

replaceBaseName :: Path rel -> LastPathPiece -> Path rel
replaceBaseName = undefined

takeDirectory :: Path rel -> Path rel
takeDirectory = undefined

replaceDirectory :: Path rel -> Path rel -> Path rel
replaceDirectory = undefined

-- | If the first path has extensions, they will be appended to the last
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
-- >>> combine "." "file.ext" :: RelPath
-- file.ext
-- >>> combine "/" "file.ext" :: AbsPath
-- /file.ext
combine :: Path rel -> RelPath -> Path rel
combine p1 p2
    | isEmptyPath p1 && isEmptyPath p2 = emptyPath
    | isEmptyPath p2 = p1
    | isEmptyPath p1 = unsafePathTypeCoerse p2
    | otherwise = Path
        { pathPieces = pathPieces p1 ++ [combineLastAndExtensions (pathLastPiece p1) (pathExtensions p1)] ++ pathPieces p2
        , pathLastPiece = pathLastPiece p2
        , pathExtensions = pathExtensions p2
        }

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
(</>) = combine

splitPath :: Path rel -> [PathPiece]
splitPath = undefined

joinPath :: [PathPiece] -> Path rel
joinPath = undefined



