module Data.Path
    ( AbsPath
    , RelPath
    , Path() -- Opaque path on purpose

    , safeRelPath
    , safeAbsPath

    , (</>)
    , (<.>)
    ) where

import Data.Path.Internal
