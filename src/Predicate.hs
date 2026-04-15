module Predicate where

import Data.List (nub)
import Data.Maybe

data PropFormula = Top | Bot | P Int | Neg PropFormula | And PropFormula PropFormula | Or PropFormula PropFormula | Impl PropFormula PropFormula | Xor PropFormula PropFormula deriving (Read, Show)

--instance Show PropFormula where
--    show Top = "T"
--    show (P x) = "(P " ++ show x ++ ")"
--    show (Neg x) = "(Neg " ++ (show x) ++ ")"
--    show (And x y) = "(And " ++ (show x) ++ " " ++ (show y) ++ ")"
--    show (Or x y) = "(Or " ++ (show x) ++ " " ++ (show y) ++ ")"
--    show (Impl x y) = "(Impl " ++ (show x) ++ " " ++ (show y) ++ ")"
--    show (Xor x y) = "(Xor " ++ (show x) ++ " " ++ (show y) ++ ")"

--Given a Universe x and a propositional formuala, determine if the statement holds
satisfy :: [Int] -> PropFormula -> Bool
satisfy _ Top = True
satisfy _ Bot = False
satisfy x (P a) = elem a x
satisfy x (Neg a) = not (satisfy x a)
satisfy x (And a b) = satisfy x a && satisfy x b
satisfy x (Or a b) = satisfy x a || satisfy x b
satisfy x (Impl a b) = satisfy x (Neg a) || satisfy x b
satisfy x (Xor a b) = (satisfy x a && satisfy x (Neg b)) || (satisfy x (Neg a) && satisfy x b)


--Returns a list of all axioms in a given propositional formula.
axiomList :: PropFormula -> [Int]
axiomList p = nub (getAxioms p) where
    getAxioms (P x) = [x]
    getAxioms Top = []
    getAxioms Bot = []
    getAxioms (Neg x) = getAxioms x
    getAxioms (And a b) = getAxioms a ++ getAxioms b
    getAxioms (Or a b) = getAxioms a ++ getAxioms b
    getAxioms (Impl a b) = getAxioms a ++ getAxioms b
    getAxioms (Xor a b) = getAxioms a ++ getAxioms b

--Given a Formula , determine a universe where the given formula is true or false depending on the boolean.
findEvaluation :: PropFormula -> Bool -> Maybe [Int]
findEvaluation p b = tryEval (axiomList p) where
    tryEval []     | satisfy [] p == b = Just []
                   | otherwise = Nothing

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
checkEquivalenceProof [] = True
checkEquivalenceProof (_:[]) = True
checkEquivalenceProof (x:y:xs) = checkEquivalent x y && checkEquivalenceProof (y:xs)
