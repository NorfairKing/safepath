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

    -- ** Inspecting extensions

    , takeExtension
    , takeExtensions

    -- ** Adding extensions

    , addExtension
    , (<.>)
    , addExtensions

    -- ** Removing extensions

    , dropExtensionExact
    , dropExtension
    , dropExtensions

    -- ** Replacing extensions

    , replaceExtensionExact
    , replaceExtension
    , (-<.>)
    , replaceExtensions
    , replaceExtensionss

    -- ** Removing given extensions

    , stripExtension
    , stripExtensions

    -- ** Splitting extensions off a path

    , splitExtension
    , splitExtensions

    -- ** Predicates involving extensions

    , hasExtension


    -- * Functions involving Path pieces

    -- ** File names

    -- *** Inspecting file names

    , takeFileNameExact
    , takeFileName

    -- *** Removing file names

    , dropFileNameExact
    , dropFileName

    -- *** Replacing file names

    , replaceFileNameExact
    , replaceFileName

    -- ** Base names

    -- *** Inspecting base names

    , takeBaseNameExact
    , takeBaseName

    -- *** Replacing base names

    , replaceBaseNameExact
    , replaceBaseName

    -- ** Directories

    -- *** Replacing directories

    , replaceDirectoryExact
    , replaceDirectory

    -- ** Combining paths

    , combineExact
    , combine
    , (</>)

    -- ** Pieces of paths

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
