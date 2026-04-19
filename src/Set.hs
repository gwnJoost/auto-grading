module Set where

import Data.List

data Set = S [Item] deriving (Show, Ord)
data Item = Set Set | Int Int | T (Item, Item) deriving (Show, Eq, Ord)
data SetConst = U Set Set | I Set Set | Diff Set Set deriving (Show)

instance Eq Set where
    (S x) == (S y) = sort (nub x) == sort (nub y)

--Given a set reduce any duplicates and sort the list.
reduce :: Set -> Set
reduce (S x) = S (sort (nub x))

--Given a set expression reduce any constructors present
simplify :: SetConst -> Set
simplify (U (S x) (S y)) = S (x ++ y)
simplify (I (S x) (S y)) = S (intersect x y)
simplify (Diff (S x) (S y)) = S (x \\ y)

--Add an item to the front of the set
prependSet :: Item -> Set -> Set
prependSet i (S x) = S (i:x)

--Concatenate two sets
concatSet :: Set -> Set -> Set
concatSet (S x) (S y) = S (x ++ y)

--Given a set calculate the powerset.
powerSet :: Set -> Set
powerSet (S []) = S [Set (S [])]
powerSet (S (x:xs)) =
  let S subsets = powerSet (S xs)     -- subset == [item]
      withX     = S [prependItemSet x s | s <- subsets] where
        prependItemSet i (Set ix) = Set (prependSet i ix)
      rest      = S subsets
  in concatSet withX rest

isSubSet :: Set -> Set-> Bool
isSubSet (S []) _ = True
isSubSet (S (x:xs)) (S y) = elem x y && isSubSet (S xs) (S y)

isProperSubSet :: Set -> Set -> Bool
isProperSubSet x y = isSubSet x y && not (x == y)

cartesian :: Set -> Set -> Set
cartesian (S a) (S b) = S (cartList a) where
  cartList [] = []
  cartList (x:xs) = (map (\y -> T (x,y)) b) ++ (cartList xs)