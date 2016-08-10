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
    , unsafeAbsPathError
    , unsafeRelPathError
    , unsafeExtError

    -- * Rendering safe paths to 'FilePath's
    , toAbsFilePath
    , toRelFilePath

    -- * Functions involving extension
    , addExtension
    , (<.>)

    , dropExtension
    , dropExtensions

    , replaceExtension
    , (-<.>)

    -- * Combining paths
    , (</>)

    ) where

import Data.Path.Internal
