module Set (Set, emptySet, setEmpty, inSet, addSet, delSet, unionSet) where

newtype Set a = Set [a] deriving (Show,Eq)

emptySet :: Set a
emptySet = Set []

setEmpty :: Set a -> Bool
setEmpty (Set []) = True
setEmpty _ = False

inSet :: (Eq a) => a -> Set a -> Bool
inSet _ (Set []) = False
inSet v (Set (x:xs))
                    | x == v = True
                    |otherwise = inSet v (Set xs)


addSet :: (Eq a) => a -> Set a -> Set a
addSet x (Set []) = Set [x]
addSet v (Set a@(x:xs))
                | inSet v (Set a) = Set a
                | otherwise = Set (v:a) 


delSet :: (Eq a) => a -> Set a -> Set a
delSet _ (Set []) = Set []
delSet v (Set (x:xs))
                | x == v = Set xs
                |otherwise = let (Set zs ) = delSet v (Set xs) in Set (x:zs)

unionSet :: (Eq a) => Set a -> Set a -> Set a
unionSet s1  (Set []) = s1
unionSet (Set []) s2 = s2
unionSet (Set (x:xs)) s2 = unionSet (Set xs) (addSet x s2)