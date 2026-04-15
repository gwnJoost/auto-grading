module Lib
    ( grader
    ) where

import Data.Maybe
import System.IO()
import Text.Read
import Control.Monad()

--other src files
import Predicate.Parse (parsePropFormula)
import Predicate

--Given a String parses said string into type propFormula
parser :: String -> String
parser s = case parsePropFormula s of
    Left err -> err
    Right p -> show p

--placeholder designed to parse input and test a certain grader.
grader :: String -> IO ()
grader a = do
    content <- readFile a
    let proof = parser content
    putStrLn proof