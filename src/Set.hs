module Set where

import Data.List (sort, nub, intersect, (\\))

data Set = S [Item] | U Set Set | I Set Set | Diff Set Set deriving (Show, Ord)
data Item = Set Set | Int Int | T (Item, Item) deriving (Show, Eq, Ord)

instance Eq Set where
    (S x) == (S y) = sort (nub x) == sort (nub y)
    x == y = (reduce x ) == (reduce y)

--Given a set reduce any duplicates and sort the list.
reduce :: Set -> Set
reduce (S x) = S (sort (nub x))
reduce (U x y) = reduce (concatSet x y)
reduce (I x y) = S (intersect rx ry) where
  (S rx) = reduce x
  (S ry) = reduce y
reduce (Diff x y) = S (rx \\ ry) where
  (S rx) = reduce x
  (S ry) = reduce y

isReduced :: Set -> Bool
isReduced (S x) = (x \\ rx) == [] where
  (S rx) = reduce (S x)
isReduced _ = False
--Add an item to the front of the set
prependSet :: Item -> Set -> Set
prependSet i (S x) = S (i:x)
prependSet i (U x y) = (U (prependSet i x) y)
prependSet i (I x y) = (I (prependSet i x) y)
prependSet i (Diff x y) = (Diff (prependSet i x) y)

--Concatenate two sets
concatSet :: Set -> Set -> Set
concatSet (S x) (S y) = S (x ++ y)
concatSet x y = concatSet (reduce x) (reduce y)

--Given a set calculate the powerset.
powerSet :: Set -> Set
powerSet (S []) = S [Set (S [])]
powerSet (S (x:xs)) =
  let S subsets = powerSet (S xs)
      withX = S [prependItemSet x s | s <- subsets] where
      prependItemSet i (Set ix) = Set (prependSet i ix)
      rest = S subsets
  in concatSet withX rest
powerSet x = powerSet (reduce x)

isSubSet :: Set -> Set-> Bool
isSubSet (S []) _ = True
isSubSet (S (x:xs)) (S y) = elem x y && isSubSet (S xs) (S y)
isSubSet x y = isSubSet (reduce x) (reduce y)

isProperSubSet :: Set -> Set -> Bool
isProperSubSet x y = isSubSet x y && not (x == y)

cartesian :: Set -> Set -> Set
cartesian (S a) (S b) = S (cartList a) where
  cartList [] = []
  cartList (x:xs) = (map (\y -> T (x,y)) b) ++ (cartList xs)
cartesian x y = cartesian (reduce x) (reduce y)