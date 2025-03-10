module Set (Set, emptySet, setEmpty, inSet, addSet, delSet, unionSet) where

newtype Set a = Set [a] deriving (Show, Eq)

-- Conjunto vacío
emptySet :: Set a
emptySet = Set []

-- Verifica si un conjunto está vacío
setEmpty :: Set a -> Bool
setEmpty (Set []) = True
setEmpty _ = False

-- Verifica si un elemento está en el conjunto
inSet :: (Eq a) => a -> Set a -> Bool
inSet _ (Set []) = False
inSet x (Set (y:ys)) 
                | x == y = True 
                | otherwise = inSet x (Set ys)

-- Agrega un elemento al conjunto (si no está presente)
addSet :: (Eq a) => a -> Set a -> Set a
addSet x (Set []) = Set [x]
addSet e s@(Set xs)
    | inSet e s = s
    | otherwise = Set (e:xs)

-- Elimina un elemento del conjunto
delSet :: (Eq a) => a -> Set a -> Set a
delSet _ (Set []) = Set []
delSet x (Set (y:ys))
    | x == y    = Set ys
    | otherwise = let (Set zs) = delSet x (Set ys) in Set (y:zs)

-- Unión de dos conjuntos usando los métodos ya definidos
unionSet :: (Eq a) => Set a -> Set a -> Set a
unionSet (Set []) s2 = s2
unionSet  s1 (Set [])= s1
unionSet (Set (x:xs)) s2 = unionSet (Set xs) (addSet x s2)