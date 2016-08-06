module Data.Path
    ( AbsPath
    , RelPath
    , Path() -- Opaque path on purpose

    , relpath
    , abspath
    , ext

    , (</>)
    , (<.>)
    , ground

    , toAbsFilePath
    , toRelFilePath
    ) where

import Data.Path.Internal
