 module Set (Set,  emptySet, setEmpty, inSet, addSet, delSet) where

newtype Set a = Set [a] deriving (Show , Eq)

emptySet :: Set a
emptySet = Set []

setEmpty :: Set a -> Bool
setEmpty (Set [])= True
setEmpty _ = False

inSet :: (Ord a) => a -> Set a -> Bool
inSet _ (Set [])= False
inSet v (Set (x:xs))
                |x == v = True
                |otherwise = inSet v (Set xs)

addSet :: (Ord a) => a -> Set a -> Set a
addSet x (Set []) = Set [x]
addSet v (Set a@(x:xs))
                   | v == x    = Set a
                   | v < x     = Set (v:a) 
                   |otherwise= let (Set zs) = addSet v (Set xs) in Set(x:zs)

delSet :: (Ord a) => a -> Set a -> Set a
delSet x (Set []) = Set [x]
delSet v (Set a@(x:xs))
                   |v < x= Set a 
                   |v > x = let (Set zs) = delSet v (Set xs) in Set(x:zs)
                   |otherwise = Set xs

unionSet :: (Ord a) => Set a -> Set a -> Set a
unionSet s1 (Set []) = s1
unionSet (Set []) s2 = s2
unionSet (Set a@(x:xs)) (Set b@(y:ys))
                                |x == y =let (Set zs) = unionSet (Set xs) (Set ys) in Set(x:zs)
                                |x < y = let (Set zs) = unionSet (Set xs) (Set b) in Set(x:zs)
                                |otherwise =let (Set zs) = unionSet (Set a) (Set ys) in Set(y:zs)