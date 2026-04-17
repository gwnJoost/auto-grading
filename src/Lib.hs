module Lib
    ( grader
    ) where

--File parses input into correct datatype then sends it to the correct autoTest.

import Data.Maybe
import System.IO
import Text.Read
import Control.Monad()

--other src files
import Parse.Parse (parsePropFormula)
import Predicate

--Given a file name reads said file and returns a formula
parser :: String -> String
parser s = case parsePropFormula s of
    Left err -> err
    Right p -> show p

--placeholder grader parses input and tests input using specified autotests.
grader :: String -> IO ()
grader a = do
    content <- readFile a
    let answer = map parser (lines content)
    putStrLn (show answer)