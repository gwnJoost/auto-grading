module Ndeduction where

import Data.List
import Data.Maybe

import Predicate

data NDProof = Proof [NDLine]  deriving (Show, Eq)
data NDLine = L (PropFormula, NDoperation, [Int]) deriving (Show, Eq)
data NDoperation = Premise | Assumption | ImplI Int Int | 
    ImplE Int Int | NegI Int Int | NegE Int Int | DNegE Int | 
    AndI Int Int | AndE Int | OrI Int | OrE Int Int Int Int Int| 
    BotE Int  deriving (Show, Eq)

findLine :: NDProof -> Int -> Maybe NDLine
findLine (Proof x) n = if length x < n then Nothing else Just (x !! (n-1))

-- Checks if two lists contain the same dependencies.
checkDep :: [Int] -> [Int] -> Bool
checkDep x y = sort (nub x) == sort (nub y) 

-- Verify if a given Natural deducuction proof is valid
validateND :: NDProof -> Bool
validateND (Proof lst) = all checkLine lst where
    checkLine (L (f, Premise, [d])) = case findLine (Proof lst) d of
                                        Nothing -> False
                                        Just l -> l == (L (f, Premise, [d]))
    checkLine (L (f, Assumption, [d])) = case findLine (Proof lst) d of
                                        Nothing -> False
                                        Just l -> l == (L (f, Assumption, [d]))
    checkLine (L (Impl fa fb, ImplI i j, d)) = case (findLine (Proof lst) i, findLine (Proof lst) j) of
                                        (Nothing, _) -> False
                                        (_, Nothing) -> False
                                        (Just (L (fa', Assumption, da)), Just (L (fb', ob, db))) -> 
                                            checkDep d (db \\ [i])
                                            && fa == fa'
                                            && fb == fb'
    checkLine (L (f, ImplE i j, d)) = case (findLine (Proof lst) i, findLine (Proof lst) j) of
                                        (Nothing, _) -> False
                                        (_, Nothing) -> False
                                        (Just (L (Impl fa f', oa, da)), Just (L (fb, ob, db))) -> 
                                            checkDep d (nub (da ++ db)) 
                                            && fa == fb
                                            && f == f'
                                        _ -> False
    checkLine (L (Neg f, NegI i j, d)) = case (findLine (Proof lst) i, findLine (Proof lst) j) of
                                        (Nothing, _) -> False
                                        (_, Nothing) -> False
                                        (Just (L (f', Assumption, da)), Just (L (Bot, ob, db))) -> 
                                            checkDep d (db \\ [i])
                                            && f == f'
                                        _ -> False
    checkLine (L (Bot, NegE i j, d)) = case (findLine (Proof lst) i, findLine (Proof lst) j) of
                                        (Nothing, _) -> False
                                        (_, Nothing) -> False
                                        (Just (L (fa, oa, da)), Just (L (Neg fb, ob, db))) -> 
                                            checkDep d (da ++ db) 
                                            && fa == fb
                                        (Just (L (Neg fa, oa, da)), Just (L (fb, ob, db))) -> 
                                            checkDep d (da ++ db)
                                            && fa == fb
                                        _ -> False
    checkLine (L (f, DNegE i, d)) = case findLine (Proof lst) i of
                                        Nothing -> False
                                        Just (L (Neg (Neg f'), oa, d')) -> 
                                            f == f'
                                            && checkDep d d'
                                        _ -> False
    checkLine (L (And fa fb, AndI i j, d)) = case (findLine (Proof lst) i, findLine (Proof lst) j) of
                                        (Nothing, _) -> False
                                        (_, Nothing) -> False
                                        (Just (L (fa', oa, da)), Just (L (fb', ob, db))) -> 
                                            checkDep d (da ++ db)
                                            && ((fa == fa' && fb == fb') || (fa == fb' && fb == fa'))
    checkLine (L (f, AndE i, d)) = case findLine (Proof lst) i of
                                        Nothing -> False
                                        Just (L (And fa fb, oa, d')) -> 
                                            (f == fa || f == fb)
                                            && checkDep d d'
                                        _ -> False
    checkLine (L (And fa fb, OrI i, d)) = case findLine (Proof lst) i of
                                        Nothing -> False
                                        Just (L (f, oa, d')) -> 
                                            checkDep d d'
                                            && (f == fa || f == fb)
    checkLine (L (f, OrE h i j k l, d)) = case (findLine (Proof lst) h, findLine (Proof lst) i, findLine (Proof lst) j) of
                                        (Nothing, _, _) -> False
                                        (_, Nothing, _) -> False
                                        (_, _, Nothing) -> False
                                        (Just (L (Or fa fb, _, dh)), Just (L (fa', Assumption, di)), Just (L (f', _, dj))) -> case (findLine (Proof lst) k, findLine (Proof lst) l) of
                                                                                                                                   (Nothing, _) -> False
                                                                                                                                   (_, Nothing) -> False
                                                                                                                                   (Just (L (fb', Assumption, dk)), Just (L (f'', _, dl))) -> 
                                                                                                                                    ((fa == fa' && fb == fb') || (fa == fb' && fb == fa'))
                                                                                                                                    && f == f' 
                                                                                                                                    && f == f''
                                                                                                                                    && checkDep d ((nub (dh ++ dj ++ dl)) \\ [i,k])
                                                                                                                                   _ -> False
                                        _ -> False
    checkLine (L (_, BotE i, d)) = case findLine (Proof lst) i of
                                        Nothing -> False
                                        Just (L (Bot, oa, d')) -> checkDep d d'
                                        _ -> False
    checkLine _ = False