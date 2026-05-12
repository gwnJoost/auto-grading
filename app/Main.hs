module Main (main) where

import Options.Applicative
import Control.Monad()

import Lib (grader)
import Parse.Parse (parsePropFormula, parseSet)
import QType (checkTF, propEquiv, setEquiv)

data Config = Config {gradertype :: String, input :: String, model :: String, points :: Float, file :: Bool}

main :: IO ()
main = runGrader =<< execParser opts where
    opts = info (configP <**> helper) (fullDesc <> progDesc "Automaticalle grade a given input." <> header "autograde - grades logical exercises")

runGrader :: Config -> IO ()
runGrader (Config g i a p f) = do
    inp <- if f then readFile i else return i
    ans <- if f then readFile a else return a
    case g of
        "checkTF" -> (grader checkTF (\x -> Right x) p inp ans)
        "propEquiv" -> (grader propEquiv parsePropFormula p inp ans)
        "setEquiv" -> (grader setEquiv parseSet p inp ans)
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
    <*> option auto
          ( long "points"
         <> short 'p'
         <> help "What is the maximum amount of points the grader should give."
         <> showDefault
         <> value 1
         <> metavar "FLOAT" )
    <*> switch
        ( long "file"
        <> short 'f'
        <> help "Choose to either read from argument or file."
        <> showDefault )