module Tests where

--File containing all tests callable by the Lib file.

import Predicate
import Set

--Predicate logic

--tests checks whether a given formula is equivalent to the model answer.
--If a difference occurs, return which predicate is different.
checkFormula :: PropFormula -> PropFormula -> Bool
checkFormula answer model = checkEquivalent answer model





--Given two expression, determine if said expresions are equivalent.
setExpEquiv :: Ord a => SetConst a -> SetConst a -> Bool
setExpEquiv x y = (simplify x) == (simplify y)