{-# LANGUAGE OverloadedStrings #-}
module SafePathTest where

import OptParse

import System.Exit (exitFailure)
import Data.Monoid ((<>))

import System.Directory
import Control.Monad.Reader
import Data.Aeson
import qualified System.FilePath as FP
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8

import TestCase
import Data.Path.Internal
import Data.Path.IO ()

safePathTest :: IO ()
safePathTest = do
    (command, settings) <- getInstructions
    runReaderT (dispatch command) settings

type Configured = ReaderT Settings IO

dispatch :: Command -> Configured ()
dispatch (CommandGenCase fp af) = genCase fp af
dispatch (CommandGenTreeCases fp ou) = genTreeCases fp ou

genTreeCases :: Maybe FilePath -> Maybe FilePath -> Configured ()
genTreeCases msp mof = do
    sp <- case msp of
        Nothing -> liftIO getCurrentDirectory
        Just sp -> return sp
    fs <- liftIO $ listDirectoryRecursive sp
    forM_ fs $ \fp ->
        genCase fp mof

listDirectoryRecursive
    :: FilePath
    -> IO [FilePath]
listDirectoryRecursive dir = do
    fs <- listDirectory dir
    fps <- (concat <$>) $ forM fs $ \file -> do
        let f = dir FP.</> file
        isFile <- doesFileExist f
        if isFile
        then return [f]
        else do
            isDir <- doesDirectoryExist f
            if isDir
            then listDirectoryRecursive f
            else return []
    return $ dir : fps


genCase :: FilePath -> Maybe FilePath -> Configured ()
genCase fp maf = do
    let mtc = case safeRelPath fp of
            Just relpath -> Just $ genRelPathCase relpath
            Nothing ->
                case safeAbsPath fp of
                    Just abspath -> Just $ genAbsPathCase abspath
                    Nothing -> Nothing
    case mtc of
        Nothing -> die $ "Invalid path: " ++ show fp
        Just bs -> liftIO $
            case maf of
                Nothing -> LB8.putStrLn bs
                Just af -> LB.appendFile af $ bs <> "\n"

die :: MonadIO m => String -> m a
die err = liftIO $ do
    putStrLn $ "ERROR: " ++ err
    exitFailure

genRelPathCase :: RelPath -> LB.ByteString
genRelPathCase p = encode $ TestCase (toRelFilePath p) p

genAbsPathCase :: AbsPath -> LB.ByteString
genAbsPathCase p = encode $ TestCase (toAbsFilePath p) p

