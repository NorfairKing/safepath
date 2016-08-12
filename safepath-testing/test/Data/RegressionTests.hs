{-# LANGUAGE OverloadedStrings #-}
module Data.RegressionTests
    ( relativePathCases
    , invalidRelativePaths
    , absolutePathCases
    , invalidAbsolutePaths
    ) where

import Data.Path.Internal
import Control.Arrow (second)


relativePathCases :: [(FilePath, RelPath)]
relativePathCases =
    map (second (\(a, b, c) -> Path (map PathPiece a) (LastPathPiece b) (map Extension c)))
    [(".", ([], "", []))
    ]

invalidRelativePaths :: [FilePath]
invalidRelativePaths =
    [ ""
    ]

absolutePathCases :: [(FilePath, AbsPath)]
absolutePathCases =
    map (second (\(a, b, c) -> Path (map PathPiece a) (LastPathPiece b) (map Extension c)))
    [("/", ([], "", []))
    ]

invalidAbsolutePaths :: [FilePath]
invalidAbsolutePaths =
    [ ""
    ]
