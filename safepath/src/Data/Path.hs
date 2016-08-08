module Data.Path
    ( AbsPath
    , RelPath
    , Path() -- Opaque path on purpose

    , relpath
    , abspath
    , ext

    , unsafeAbsPathError
    , unsafeRelPathError

    , (</>)

    , addExtension
    , (<.>)

    , ground

    , toAbsFilePath
    , toRelFilePath

    , dropExtension
    , dropExtensions

    , replaceExtension
    , (-<.>)

    , takeLastPiece
    ) where

import Data.Path.Internal
