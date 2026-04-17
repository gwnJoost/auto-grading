module Set where

import Data.List

data Set a = S [a] deriving (Show)
data SetConst a = U (Set a) (Set a) | I (Set a) (Set a) | Diff (Set a) (Set a)

instance Ord a => Eq (Set a) where
    (S x) == (S y) = sort (nub x) == sort (nub y)

--Given a set reduce any duplicates and sort the list.
reduce :: Ord a => Set a -> Set a
reduce (S x) = S (sort (nub x))

--Given a set expression reduce any constructors present
simplify :: Eq a => SetConst a -> Set a
simplify (U (S x) (S y)) = S (x ++ y)
simplify (I (S x) (S y)) = S (intersect x y)
simplify (Diff (S x) (S y)) = S (x \\ y)

--Add an item to the front of the set
prependSet :: Eq a => a -> Set a -> Set a
prependSet a (S x) = S (a:x)

--Concatenate two sets
concatSet :: Eq a => Set a -> Set a -> Set a
concatSet (S x) (S y) = S (x ++ y)

--Given a set calculate the powerset.
powerSet :: Ord a => Set a -> Set (Set a)
powerSet (S []) = S [(S [])]
powerSet (S (x:xs)) =
  let S subsets = powerSet (S xs)
      withX     = S [prependSet x s | s <- subsets]
      rest      = S subsets
  in concatSet withX rest

isSubSet :: Eq a => Set a -> Set a -> Bool
isSubSet (S []) _ = True
isSubSet (S (x:xs)) (S y) = elem x y && isSubSet (S xs) (S y)

isProperSubSet :: Ord a => Set a -> Set a -> Bool
isProperSubSet x y = isSubSet x y && not (x == y)

cartesian :: Ord a => Set a -> Set b -> Set (a,b)
cartesian (S a) (S b) = S (cartList a) where
  cartList [] = []
  cartList (x:xs) = (map (\y -> (x,y)) b) ++ (cartList xs)