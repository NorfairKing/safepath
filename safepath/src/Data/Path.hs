module Data.Path
    ( AbsPath
    , RelPath
    , Path() -- Opaque path on purpose

    , relpath
    , abspath

    , (</>)
    , (<.>)
    ) where

import Data.Path.Internal
