-- con listas ordenadas

module Set (Set, emptySet, setEmpty, inSet, addSet, delSet, unionSet) where

newtype Set a = S [a] deriving (Show, Eq)

-- Crear un conjunto vacío
emptySet :: Set a
emptySet = S []

-- Verificar si un conjunto está vacío
setEmpty :: Set a -> Bool
setEmpty (S []) = True
setEmpty _      = False

-- Verificar si un elemento está en el conjunto
inSet :: (Eq a) => a -> Set a -> Bool
inSet _ (S []) = False
inSet x (S (y:ys))
    | x == y    = True
    | otherwise = inSet x (S ys)

-- Insertar un elemento en un conjunto ordenado (manteniendo el orden y sin duplicados)
addSet :: (Ord a) => a -> Set a -> Set a
addSet x (S []) = S [x]
addSet x (S ys@(y:ys'))
    | x == y    = S ys  -- Si ya existe, no lo insertamos
    | x < y     = S (x:ys)  -- Insertamos antes de `y` para mantener orden
    | otherwise = let S zs = addSet x (S ys') in S (y:zs)

-- Eliminar un elemento de un conjunto
delSet :: (Ord a) => a -> Set a -> Set a
delSet _ (S []) = S []
delSet x (S (y:ys))
    | x == y    = S ys
    | x < y     = S (y:ys)  -- Si `x` no está en el conjunto, lo dejamos igual
    | otherwise = let S zs = delSet x (S ys) in S (y:zs)

-- Unir dos conjuntos ordenados sin duplicados
unionSet :: (Ord a) => Set a -> Set a -> Set a
unionSet (S []) s2 = s2
unionSet s1 (S []) = s1
unionSet (S (x:xs)) (S (y:ys))
    | x == y    = let S zs = unionSet (S xs) (S ys) in S (x:zs)
    | x < y     = let S zs = unionSet (S xs) (S (y:ys)) in S (x:zs)
    | otherwise = let S zs = unionSet (S (x:xs)) (S ys) in S (y:zs)