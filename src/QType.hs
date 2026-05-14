module QType where

--File containing all tests callable by the Lib file.

import Predicate
import Set
import Ndeduction

--general questions

-- Check if the given answer is equal to the desired answer
-- 100% points if equal
-- 0% points otherwise
checkTF :: String -> String -> Float -> Float
checkTF x y points = if x == y then points else 0


--Predicate logic

-- tests checks whether a given formula is equivalent to the model answer.
-- 100% if both are equal
-- 50% if equivalent but not equal
-- 0% otherwise
propEquiv :: PropFormula -> PropFormula -> Float -> Float
propEquiv x y points | checkEquivalent x y && (complexity x == complexity y) = points
                     | checkEquivalent x y = points / 2
                     | otherwise = 0


--Set Theory

--Given Sets, check if the two sets are equivalent.
-- 100% if both are equal.
-- 50% if equal, but answer contains duplicates.
-- 0% otherwise
setEquiv :: Set -> Set -> Float -> Float
setEquiv x y points | (reduce x) == (reduce y) && isReduced x = points
                    | (reduce x) == (reduce y) = points / 2
                    | otherwise = 0

-- Given Natural deduction proofs, check if the input proof x is a valid proof and follows the requirements of proof y.
-- Proof y should have the following structure:
--  1. first lines should contain all the premises.
--  2. the final line should contain the resulting formula to be reduced to.
verifyND :: NDProof -> NDProof -> Float -> Float
verifyND (Proof x) (Proof y) points | all validateCorrectProof x && validateND (Proof x) = points
                                    | otherwise = 0 where
    validateCorrectProof (L (f, Premise, d)) = elem (L (f, Premise, d)) y
    validateCorrectProof (L (f, o, d)) = if (L (f, o, d)) == last x then fx == fy && dx == dy else True where
        (L (fy, oy, dy)) = last y
        (L (fx, ox, dx)) = last x
