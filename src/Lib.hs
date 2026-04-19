module Lib
    ( grader
    ) where

--File parses input into correct datatype then sends it to the correct autoTest.

import Data.Maybe
import System.IO
import Text.Read
import Control.Monad()

--other src files
import Parse.Parse (parsePropFormula, parseSet, parseSetConst)
import Predicate
import Set

--Given a file name reads said file and returns a formula
parser :: Show a => String -> (String -> Either String a) -> String
parser s f = case f s of
    Left err -> err
    Right p -> show p

--placeholder grader parses input and tests input using specified autotests.
grader :: String -> IO ()
grader a = do
    content <- readFile a
    let answer = map (\x -> parser x parseSetConst) (lines content)
    putStrLn (show answer)