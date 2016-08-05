{-# LANGUAGE OverloadedStrings #-}
module Data.PathCases
  ( relativePathCases
  , absolutePathCases
  ) where

import Data.Path.Internal
import Control.Arrow (first)

pathCases :: [(String, Path rel)]
pathCases = map (\(a, b, c, d) -> (a, Path (map PathPiece b) (LastPathPiece $ PathPiece c) (map Extension d)))
  [ ("test", [], "test", [])
  , ("test/file", ["test"], "file", [])
  , ("test/file/path", ["test", "file"], "path", [])
  , ("test/file/path.ext", ["test", "file"], "path", ["ext"])
  ]

relativePathCases :: [(String, RelPath)]
relativePathCases = pathCases

absolutePathCases :: [(String, AbsPath)]
absolutePathCases = map (first ('/':)) pathCases
