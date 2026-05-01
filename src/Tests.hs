module Tests where

--File containing all tests callable by the Lib file.

import Predicate
import Set

--general questions

--Check if the given answer is equal to the desired answer
checkTF :: String -> String -> Bool
checkTF x y = x == y


--Predicate logic

--tests checks whether a given formula is equivalent to the model answer.
--If a difference occurs, return which predicate is different.
checkFormula :: PropFormula -> PropFormula -> Bool
checkFormula answer model = checkEquivalent answer model



--Set Theory



--Given two expression, determine if said expresions are equivalent.


