{-# LANGUAGE OverloadedStrings #-}
module Data.PathCases
  ( relativePathCases
  , absolutePathCases
  ) where

import Data.Path.Internal
import Control.Arrow (first)

pathCases :: [(String, Path rel)]
pathCases = map (\(a, b, c) -> (a, Path (map PathPiece b) (map Extension c)))
  [ ("test", ["test"], [])
  , ("test/file", ["test", "file"], [])
  , ("test/file/path", ["test", "file", "path"], [])
  , ("test/file/path.ext", ["test", "file", "path"], ["ext"])
  ]

relativePathCases :: [(String, RelPath)]
relativePathCases = pathCases

absolutePathCases :: [(String, AbsPath)]
absolutePathCases = map (first ('/':)) pathCases
