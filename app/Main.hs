module Main (main) where

import System.Environment
import Options.Applicative
import Text.Read
import Control.Monad()

import Lib (grader)
import Parse.Parse (parsePropFormula, parseSet)
import Tests (checkTF)

data Config = Config {gradertype :: String, input :: String, model :: String, file :: Bool}

main :: IO ()
main = runGrader =<< execParser opts where
    opts = info (configP <**> helper) (fullDesc <> progDesc "Automaticalle grade a given input." <> header "autograde - grades logical exercises")

runGrader :: Config -> IO ()
runGrader (Config g i a f) = do
    input <- if f then readFile i else return i
    answer <- if f then readFile a else return a
    case g of
        "checkTF" -> (grader checkTF (\x -> Right x) input answer)
        _ -> putStrLn "Grader does not exist"

configP :: Parser Config
configP = Config
    <$> strOption
        ( long "grader"
        <> short 'g'
        <> help "Select which grader to use."
        <> metavar "GRADE" )
    <*> strOption
        ( long "input"
        <> short 'i'
        <> help "The input file with answer to grade."
        <> metavar "INPUT" )
    <*> strOption
        ( long "answer"
        <> short 'a'
        <> help "The desired model answer."
        <> showDefault
        <> metavar "ANSWER" )
    <*> switch
        ( long "file"
        <> short 'f'
        <> help "Choose to either read from argument or file."
        <> showDefault )