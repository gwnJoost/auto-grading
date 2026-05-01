module Predicate where

--File contains definition of predicate logic and various function used for predicate logic

import Data.List (nub)
import Data.Maybe

data PropFormula = Top | Bot | P Int | Neg PropFormula | And PropFormula PropFormula | Or PropFormula PropFormula | Impl PropFormula PropFormula | Xor PropFormula PropFormula deriving (Read, Show)

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
predicateList :: PropFormula -> [Int]
predicateList p = nub (getPredicate p) where
    getPredicate (P x) = [x]
    getPredicate Top = []
    getPredicate Bot = []
    getPredicate (Neg x) = getPredicate x
    getPredicate (And a b) = getPredicate a ++ getPredicate b
    getPredicate (Or a b) = getPredicate a ++ getPredicate b
    getPredicate (Impl a b) = getPredicate a ++ getPredicate b
    getPredicate (Xor a b) = getPredicate a ++ getPredicate b

-- Given a Formula , determine a universe where the given formula is true or
-- false depending on the boolean.
findEvaluation :: PropFormula -> Maybe [Int]
findEvaluation p = tryEval (predicateList p) where
    tryEval []     | satisfy [] p = Just []
                   | otherwise = Nothing

    tryEval (x:[]) | satisfy [x] p = Just [x]
                   | satisfy [] p = Just []
                   | otherwise = Nothing

    tryEval (x:xs) | satisfy (x:xs) p = Just (x:xs)
                   | satisfy xs p = Just xs
                   | otherwise = tryEval (x: tail xs)


-- | Checks whether two statements are equivalent by checking if an evaluation
-- | exists for which one holds but the other does not.
checkEquivalent :: PropFormula -> PropFormula -> Bool
checkEquivalent f1 f2 = isNothing (findEvaluation (Xor f1 f2))


-- | Check if a proof by equivalence transformations is correct
checkEquivalenceProof :: [PropFormula] -> Bool
checkEquivalenceProof [] = True
checkEquivalenceProof (_:[]) = True
checkEquivalenceProof (x:y:xs) = checkEquivalent x y && checkEquivalenceProof (y:xs)
