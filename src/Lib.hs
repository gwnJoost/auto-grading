module Lib
    ( grader
    ) where

--File parses input into correct datatype then sends it to the correct autoTest.
grader :: Show a => (a -> a -> Float -> Float) -> (String -> Either (Int, Int) a) -> Float -> String -> String -> IO ()
grader grade parse points input answer = do
    case (parse input, parse answer) of
        (Left (r, c), _) -> putStrLn ("Could not parse input at " ++ show (r,c) ".")
        (_, Left (r, c)) -> putStrLn ("Could not parse model answer at " ++ show (r,c) ++ ".")
        (Right p, Right q) -> putStrLn ("Grading complete, got: " ++ show (grade p q points) ++ " out of " ++ show points ++ " points.")