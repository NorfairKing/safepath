module Data.Path
    (
    -- * Safe Path Types

      AbsPath
    , RelPath
    , Path() -- Opaque path on purpose

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
    , takeExtensions

    , replaceExtension
    , replaceExtensions
    , replaceExtensionss
    , (-<.>)

    , dropExtension
    , dropExtensions

    , addExtension
    , (<.>)

    , stripExtension
    , stripExtensions

    , splitExtensions

    , hasExtension

    -- * Combining paths
    , (</>)

    ) where

import Data.Path.Internal
