module Lib
    ( reader
    ) where

import Data.List (nub)
import Data.Maybe
import System.IO
import Text.Read
import Control.Monad

data PropFormula = Top | A Int | Neg PropFormula | And PropFormula PropFormula | Or PropFormula PropFormula | Impl PropFormula PropFormula | Xor PropFormula PropFormula deriving (Read)

instance Show PropFormula where
    show Top = "T"
    show (A x) = "(A " ++ show x ++ ")"
    show (Neg x) = "(Neg " ++ (show x) ++ ")"
    show (And x y) = "(And " ++ (show x) ++ " " ++ (show y) ++ ")"
    show (Or x y) = "(Or " ++ (show x) ++ " " ++ (show y) ++ ")"
    show (Impl x y) = "(Impl " ++ (show x) ++ " " ++ (show y) ++ ")"
    show (Xor x y) = "(Xor " ++ (show x) ++ " " ++ (show y) ++ ")"

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


--Checks whether two statements are equivalent
checkEquivalent :: PropFormula -> PropFormula -> Bool
checkEquivalent f1 f2 = isNothing (findEvaluation (Xor f1 f2) True)


--Check if a proof by equivalence transformations is correct
checkEquivalenceProof :: [PropFormula] -> Bool
checkEquivalenceProof (x:[]) = True
checkEquivalenceProof (x:xs) = checkEquivalent x (head xs) && checkEquivalenceProof xs

--placeholder designed to parse input and test a certain grader.
reader :: String -> IO ()
reader a = do 
    content <- readFile a
    --let proof = fmap readMaybe (lines content) :: [Maybe PropFormula]
    let proof = fmap readMaybe (lines content) :: [Maybe PropFormula]
    putStrLn (show (map fromJust proof))
    putStrLn (show (checkEquivalenceProof (map fromJust proof)))