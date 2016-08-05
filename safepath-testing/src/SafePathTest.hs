module SafePathTest where

import OptParse

import System.Exit (exitFailure)

import Control.Monad.Reader
import Data.Aeson
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
                Just af -> LB.appendFile af bs

die :: MonadIO m => String -> m a
die err = liftIO $ do
    putStrLn $ "ERROR: " ++ err
    exitFailure

genRelPathCase :: RelPath -> LB.ByteString
genRelPathCase p = encode $ TestCase (toRelFilePath p) p

genAbsPathCase :: AbsPath -> LB.ByteString
genAbsPathCase p = encode $ TestCase (toAbsFilePath p) p

