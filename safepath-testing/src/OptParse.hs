module OptParse where

import Options.Applicative

import System.Environment
import System.Exit (exitFailure)

type Instructions = (Command, Settings)

data Settings
    = Settings
    deriving (Show, Eq)

defaultSettings :: Settings
defaultSettings = Settings

data Command
    = CommandGenCase
        FilePath
        (Maybe FilePath)
          -- ^ The sample filepath
          -- ^ The data file to append to
    | CommandGenTreeCases
        (Maybe FilePath)
        (Maybe FilePath)
          -- ^ The starting directory
          -- ^ The data file to write to
    deriving (Show, Eq)

data Flags
    = Flags

getInstructions :: IO Instructions
getInstructions = do
    args <- getArguments
    case combineToInstructions args of
        Left err -> do
            putStrLn $ "Failed to load instructions :" ++ err
            exitFailure
        Right instr -> return instr


combineToInstructions :: Arguments -> Either String Instructions
combineToInstructions
    ( command
    , Flags
    )
    = Right (command, Settings)


type Arguments = (Command, Flags)

getArguments :: IO Arguments
getArguments = do
    args <- getArgs
    let result = runArgumentsParser args
    handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs argParser
  where
    prefs = ParserPrefs
      { prefMultiSuffix = "SAFEPATHTESTTOOL"
      , prefDisambiguate = True
      , prefShowHelpOnError = True
      , prefBacktrack = True
      , prefColumns = 80
      }

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) help
  where
    help = fullDesc <> progDesc description
    description = "Safepath Test Tool"

parseArgs :: Parser Arguments
parseArgs = (,) <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat
    [ command "gencase"       $ parseGenCase
    , command "gentreecases"  $ parseGenTreeCases
    ]

parseGenCase :: ParserInfo Command
parseGenCase = info parser modifier
  where
    parser = CommandGenCase
        <$> strArgument (metavar "FILEPATH"
            <> help "the path to generate a test case for")
        <*> option (Just <$> str) (metavar "CASEFILE"
            <> value Nothing
            <> short 'a'
            <> long "amend"
            <> help "the path to the data file to append to")
    modifier = fullDesc
            <> progDesc "Generate a single test case"

parseGenTreeCases :: ParserInfo Command
parseGenTreeCases = info parser modifier
  where
    parser = CommandGenTreeCases
        <$> option (Just <$> str) (metavar "ROOT"
            <> value Nothing
            <> short 'r'
            <> long "root"
            <> help "the path to root to start from")
        <*> option (Just <$> str) (metavar "CASEFILE"
            <> value Nothing
            <> short 'o'
            <> long "output"
            <> help "the path to the data file to output to")
    modifier = fullDesc
            <> progDesc "Generate a single test case"


parseFlags :: Parser Flags
parseFlags = pure Flags

