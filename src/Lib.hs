module Lib
    ( grader
    ) where

--File parses input into correct datatype then sends it to the correct autoTest.

import Data.Maybe
import System.IO
import Text.Read
import Control.Monad()

--other src files
import Parse.Parse (parsePropFormula, parseSet)
import Tests (checkTF)

--placeholder grader parses input and tests input using specified autotests.
grader :: (a -> a -> Float -> Float) -> (String -> Either (Int, Int) a) -> Float -> String -> String -> IO ()
grader grade parse points input answer = do
    case (parse input, parse answer) of
        (Left (r, c), _) -> putStrLn ("Could not parse input at row " ++ (show r) ++ " and col " ++ (show c) ++ ".")
        (_, Left (r, c)) -> putStrLn ("Could not parse answer at row " ++ (show r) ++ " and col " ++ (show c) ++ ".")
        (Right p, Right q) -> putStrLn ("Grading complete, got: " ++ show (grade p q points) ++ " out of " ++ show points ++ " points.")