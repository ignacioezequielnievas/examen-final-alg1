-- set como arbol binario

module Set (Set, emptySet, setEmpty, inSet, addSet, delSet, unionSet) where

-- Definimos el tipo Set como un Árbol Binario de Búsqueda (BST)
data Set a = Vacio | Nodo a (Set a) (Set a) deriving (Show, Eq)

-- Crear un conjunto vacío
emptySet :: Set a
emptySet = Vacio

-- Verificar si un conjunto está vacío
setEmpty :: Set a -> Bool
setEmpty Vacio = True
setEmpty _     = False

-- Verificar si un elemento está en el conjunto
inSet :: (Ord a) => a -> Set a -> Bool
inSet _ Vacio = False
inSet x (Nodo v izq der)
    | x == v    = True
    | x < v     = inSet x izq
    | otherwise = inSet x der

-- Insertar un elemento en el árbol sin duplicados
addSet :: (Ord a) => a -> Set a -> Set a
addSet x Vacio = Nodo x Vacio Vacio
addSet x (Nodo v izq der)
    | x == v    = Nodo v izq der  -- No se insertan duplicados
    | x < v     = Nodo v (addSet x izq) der
    | otherwise = Nodo v izq (addSet x der)

-- Eliminar un elemento del árbol
delSet :: (Ord a) => a -> Set a -> Set a
delSet _ Vacio = Vacio
delSet x (Nodo v izq der)
    | x < v     = Nodo v (delSet x izq) der
    | x > v     = Nodo v izq (delSet x der)
    | otherwise = eliminarNodo (Nodo v izq der)  -- Caso cuando encontramos el valor

-- Función auxiliar para eliminar un nodo del árbol
eliminarNodo :: (Ord a) => Set a -> Set a
eliminarNodo (Nodo _ Vacio der) = der  -- Si no hay hijo izquierdo, devolvemos el derecho
eliminarNodo (Nodo _ izq Vacio) = izq  -- Si no hay hijo derecho, devolvemos el izquierdo
eliminarNodo (Nodo _ izq der)   = Nodo minValor izq (delSet minValor der)
    where minValor = minElemento der  -- Reemplazar con el menor elemento del subárbol derecho

-- Función auxiliar para encontrar el mínimo elemento en un árbol
minElemento ::(Ord a) => Set a -> a
lowTreeSet Empty = error "arbol vacio"
minElemento (Nodo v Vacio _) = v
minElemento (Nodo _ izq _)   = minElemento izq

-- Unir dos conjuntos usando los métodos definidos
unionSet :: (Ord a) => Set a -> Set a -> Set a
unionSet Vacio s2 = s2
unionSet s1 Vacio = s1
unionSet (Nodo v izq der) s2 = addSet v (unionSet izq (unionSet der s2))