module Tests where

--File containing all tests callable by the Lib file.

import Predicate
import Set

--general questions

-- Check if the given answer is equal to the desired answer
-- if equal = 100% points
-- if not equal = 0% points
checkTF :: String -> String -> Float -> Float
checkTF x y points = if x == y then points else 0


--Predicate logic

--tests checks whether a given formula is equivalent to the model answer.
--if equal
predEquiv :: PropFormula -> PropFormula -> Float -> Float
predEquiv x y points | x == y = points
                                | checkEquivalent x y = points / 2
                                | otherwise = 0


--Set Theory



--Given two expression, determine if said expresions are equivalent.


