-- con listas no ordenadas
module Set (Set, emptySet, setEmpty, inSet, addSet, delSet, unionSet) where

-- Definimos el tipo de dato Set como una lista
newtype Set a = Set [a] deriving (Show, Eq)

-- Crea un conjunto vacío
emptySet :: Set a
emptySet = Set []

-- Verifica si un conjunto está vacío
setEmpty :: Set a -> Bool
setEmpty (Set xs) = null xs

-- Verifica si un elemento está en el conjunto
inSet :: (Eq a) => a -> Set a -> Bool
inSet x (Set xs) = x `elem` xs

-- Agrega un elemento al conjunto si no está presente
addSet :: (Eq a) => a -> Set a -> Set a
addSet x s@(Set xs)
  | x `inSet` s = s   -- Si ya está, devuelve el conjunto sin cambios
  | otherwise   = Set (x:xs)  -- Lo agrega al frente

-- Elimina un elemento del conjunto
delSet :: (Eq a) => a -> Set a -> Set a
delSet x (Set xs) = Set (filter (/= x) xs)

-- Une dos conjuntos sin duplicados
unionSet :: (Eq a) => Set a -> Set a -> Set a
unionSet (Set xs) (Set ys) = foldr addSet (Set xs) ys