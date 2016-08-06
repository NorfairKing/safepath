{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
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

data AbsPath
    = AbsPath
        [PathPiece]
        LastPathPiece
        [Extension]
    deriving (Show, Eq, Generic, Data, Typeable)

instance Validity AbsPath where
    isValid (AbsPath pp lp es)
        =  isValid pp
        && isValid lp
        && isValid es

data RelPath
    = RelPath
        [PathPiece]
        LastPathPiece
        [Extension]
    deriving (Show, Eq, Generic, Data, Typeable)

instance Validity RelPath where
    isValid (RelPath pp lp es)
        =  isValid pp
        && isValid lp
        && isValid es

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
instance IsString AbsPath where
    fromString = unsafeAbsPathError

-- | ONLY for @OverloadedStrings@
instance IsString RelPath where
    fromString = unsafeRelPathError

class Path p where
    join    :: p -> RelPath -> p
    addext  :: p -> Extension -> p


containsSatisfied :: (Char -> Bool) -> Text -> Bool
containsSatisfied func = isJust . T.find func

containsSeparator :: Text -> Bool
containsSeparator = containsSatisfied (== '/')

containsDot :: Text -> Bool
containsDot = containsSatisfied (== '.')

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
    return $ RelPath pieces lastPiece exts
  where
    unsnocMay :: [a] -> Maybe ([a], a)
    unsnocMay as = (,) <$> initMay as <*> lastMay as
    unconsMay :: [a] -> Maybe (a, [a])
    unconsMay [] = Nothing
    unconsMay (a:as) = Just (a, as)


unsafeRelPathError :: FilePath -> RelPath
unsafeRelPathError = constructValidUnsafe . fromMaybe (error "invalid relative path") . safeRelPath

safeAbsPath :: FilePath -> Maybe AbsPath
safeAbsPath [] = Nothing
safeAbsPath ('/':fp) = do
    RelPath ps lp es <-safeRelPath fp
    return $ AbsPath ps lp es
safeAbsPath _ = Nothing

unsafeAbsPathError :: FilePath -> AbsPath
unsafeAbsPathError = constructValidUnsafe . fromMaybe (error "invalid absolute path") . safeAbsPath

toRelFilePath :: RelPath -> FilePath
toRelFilePath (RelPath pp lp es)
    =  intercalate "/" (map renderPiece $ pp ++ [lpp])
    ++ renderExtensions es
  where (LastPathPiece lpp) = lp

toAbsFilePath :: AbsPath -> FilePath
toAbsFilePath (AbsPath ps lp es) = ('/':) . toRelFilePath $ rp
  where rp = RelPath ps lp es

lastPieceToPiece :: LastPathPiece -> PathPiece
lastPieceToPiece (LastPathPiece p) = p

renderPiece :: PathPiece -> String
renderPiece (PathPiece p) = T.unpack p

renderLastPiece :: LastPathPiece -> String
renderLastPiece = renderPiece . lastPieceToPiece

renderExtension :: Extension -> String
renderExtension (Extension e) = T.unpack e

renderExtensions :: [Extension] -> String
renderExtensions [] = ""
renderExtensions es = "." ++ intercalate "." (map renderExtension es)

combineLastAndExtensions :: LastPathPiece -> [Extension] -> PathPiece
combineLastAndExtensions (LastPathPiece (PathPiece lpp)) es
    = PathPiece $ lpp <> T.pack (renderExtensions es)

joinRel :: RelPath -> RelPath -> RelPath
joinRel (RelPath ps1 lp1 es1) (RelPath ps2 lp2 es2)
    = RelPath
    (ps1 ++ [combineLastAndExtensions lp1 es1] ++ ps2)
    lp2
    es2


joinAbs :: AbsPath -> RelPath -> AbsPath
joinAbs (AbsPath ps1 lp1 es1) (RelPath ps2 lp2 es2)
    = AbsPath
    (ps1 ++ [combineLastAndExtensions lp1 es1] ++ ps2)
    lp2
    es2

addExtRel :: RelPath -> Extension -> RelPath
addExtRel (RelPath ps lp es) extension
    = RelPath ps lp (es ++ [extension])

addExtAbs :: AbsPath -> Extension -> AbsPath
addExtAbs (AbsPath ps lp es) extension
    = AbsPath ps lp (es ++ [extension])

(</>) :: Path path => path -> RelPath -> path
(</>) = join

(<.>) :: Path path => path -> Extension -> path
(<.>) = addext

