module Tests where

--File containing all tests callable by the Lib file.

import Predicate
import Set

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
predEquiv :: PropFormula -> PropFormula -> Float -> Float
predEquiv x y points | x == y = points
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


