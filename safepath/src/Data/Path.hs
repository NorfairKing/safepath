module Data.Path
    (
    -- * Safe Path Types

      Path() -- Opaque path on purpose

    , AbsPath
    , RelPath
    , Absolute() -- Phantom anyway
    , Relative() -- Phantom anyway

    -- ** Safe path helper types

    , PathPiece()     -- Opaque path on purpose
    , LastPathPiece() -- Opaque path on purpose
    , Extension()     -- Opaque path on purpose


    -- * Constructing safe values

    -- ** Constructing safe values safely

     , relpath
    , abspath
    , ext
    , ground

    -- ** Constructing safe values unsafely

    , unsafeRelPathError
    , unsafeAbsPathError
    , unsafeExtError


    -- * Rendering safe paths to 'FilePath's

    , toRelFilePath
    , toAbsFilePath


    -- * Functions involving extension

    , takeExtension
    , takeExtensions

    , replaceExtension
    , (-<.>)
    , replaceExtensions
    , replaceExtensionss

    , dropExtension
    , dropExtensions

    , addExtension
    , (<.>)
    , addExtensions

    , stripExtension
    , stripExtensions

    , splitExtension
    , splitExtensions

    , hasExtension


    -- * Functions involving Path pieces

    , takeFileNameExact
    , takeFileName

    , replaceFileNameExact
    , replaceFileName

    , dropFileNameExact
    , dropFileName

    , takeBaseNameExact
    , takeBaseName

    , replaceBaseNameExact
    , replaceBaseName

    , replaceDirectoryExact
    , replaceDirectory

    , combineExact
    , combine
    , (</>)

    , splitPath
    , joinPath


    -- * Separators

    -- ** Paths

    , pathSeparator
    , pathSeparators
    , isPathSeparator

    -- ** Extension

    , extensionSeparator
    , extensionSeparators
    , isExtensionSeparator

    ) where

import Data.Path.Internal
