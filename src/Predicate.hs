module Predicate where

--File contains definition of predicate logic and various function used for predicate logic

import Data.List (nub, subsequences)
import Data.Maybe

data PropFormula = Top | Bot | P Int | Neg PropFormula | And PropFormula PropFormula | Or PropFormula PropFormula | Impl PropFormula PropFormula | Xor PropFormula PropFormula deriving (Eq, Read, Show)

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
findEvaluation p = findEval allEvals where
    allEvals = subsequences (predicateList p)
    findEval [] = if satisfy [] p then Just [] else Nothing
    findEval(x:xs) = if satisfy x p then Just x else findEval xs


-- | Checks whether two statements are equivalent by checking if an evaluation
-- | exists for which one holds but the other does not.
checkEquivalent :: PropFormula -> PropFormula -> Bool
checkEquivalent f1 f2 = isNothing (findEvaluation (Xor f1 f2))


-- | Check if a proof by equivalence transformations is correct
checkEquivalenceProof :: [PropFormula] -> Bool
checkEquivalenceProof [] = True
checkEquivalenceProof (_:[]) = True
checkEquivalenceProof (x:y:xs) = checkEquivalent x y && checkEquivalenceProof (y:xs)

-- Given a propositional formula, determine the complexity of the formula.
complexity :: PropFormula -> Int
complexity (P x) = 0
complexity Top = 0
complexity Bot = 0
complexity (Neg x) = 1 + (complexity x)
complexity (And x y) = 1 + (complexity x) + (complexity y)
complexity (Or x y) = 1 + (complexity x) + (complexity y)
complexity (Xor x y) = 1 + (complexity x) + (complexity y)
complexity (Impl x y) = 1 + (complexity x) + (complexity y)

