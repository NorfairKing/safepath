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
    , (<.>)
    , ground

    , toAbsFilePath
    , toRelFilePath

    , removeExtensions
    , takeLastPiece
    ) where

import Data.Path.Internal
