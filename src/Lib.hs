module Lib
    ( grader
    ) where

import Data.List (nub)
import Data.Maybe
import System.IO
import Text.Read
import Control.Monad

data PropFormula = Top | A Int | Neg PropFormula | And PropFormula PropFormula | Or PropFormula PropFormula | Impl PropFormula PropFormula | Xor PropFormula PropFormula  deriving (Read)

instance Show PropFormula where
    show Top = "T"
    show (A x) = show x
    show (Neg x) = "-" ++ (show x)
    show (And x y) = "(" ++ (show x) ++ " and " ++ (show y) ++ ")"
    show (Or x y) = "(" ++ (show x) ++ " or " ++ (show y) ++ ")"
    show (Impl x y) = "(" ++ (show x) ++ " -> " ++ (show y) ++ ")"
    show (Xor x y) = "(" ++ (show x) ++ " xor " ++ (show y) ++ ")"

--Given a Universe x and a propositional formuala, determine if the statement holds
satisfy :: [Int] -> PropFormula -> Bool
satisfy x Top = True
satisfy x (A a) = elem a x
satisfy x (Neg a) = not (satisfy x a)
satisfy x (And a b) = satisfy x a && satisfy x b
satisfy x (Or a b) = satisfy x a || satisfy x b
satisfy x (Impl a b) = satisfy x (Neg a) || satisfy x b
satisfy x (Xor a b) = (satisfy x a && satisfy x (Neg b)) || (satisfy x (Neg a) && satisfy x b)


--Returns a list of all axioms in a given propositional formula.
axiomList :: PropFormula -> [Int]
axiomList x = nub (getAxioms x) where
    getAxioms (A x) = [x]
    getAxioms Top = []
    getAxioms (Neg x) = getAxioms x
    getAxioms (And a b) = getAxioms a ++ getAxioms b
    getAxioms (Or a b) = getAxioms a ++ getAxioms b
    getAxioms (Impl a b) = getAxioms a ++ getAxioms b
    getAxioms (Xor a b) = getAxioms a ++ getAxioms b

--Given a Formula , determine a universe where the given formula is true or false depending on the boolean.
findEvaluation :: PropFormula -> Bool -> Maybe [Int]
findEvaluation p b = tryEval (axiomList p) where
    tryEval (x:[]) | satisfy [x] p == b = Just [x]
                   | satisfy [] p == b = Just []
                   | otherwise = Nothing

    tryEval (x:xs) | satisfy (x:xs) p == b = Just (x:xs)
                   | satisfy xs p == b = Just xs
                   | otherwise = tryEval (x: tail xs)


checkEquivalent :: PropFormula -> PropFormula -> Bool
checkEquivalent f1 f2 = isNothing (findEvaluation (Or (And f1 (Neg f2)) (And (Neg f1) f2)) True)

--placeholder grader that reads in a file and gives a valid output.
grader :: IO ()
grader =  do 
    putStrLn "Give File"
    file <- getLine
    content <- readFile file
    putStrLn content
    case findEvaluation (read content) False of
        Just x -> putStrLn (show x)
        Nothing -> putStrLn "Impossible"