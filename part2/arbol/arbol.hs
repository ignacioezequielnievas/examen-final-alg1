-- arbol pre-in-post orden
data ArbolBin a = Vacio | Nodo a (ArbolBin a) (ArbolBin a) deriving (Show, Eq)


mkNewTree :: (Ord a) => ArbolBin a
mkNewTree = Vacio


-- Agregar un elemento al árbol
addTree :: (Ord a) => a -> ArbolBin a -> ArbolBin a
addTree x Vacio = Nodo x Vacio Vacio  -- Si el árbol está vacío, se crea un nodo con el valor x
addTree x (Nodo v izq der)
    | x < v     = Nodo v (addTree x izq) der  -- Insertar en el subárbol izquierdo si es menor
    | x > v     = Nodo v izq (addTree x der)  -- Insertar en el subárbol derecho si es mayor
    | otherwise = Nodo v izq der  -- Si es igual, no se inserta (evita duplicados)

-- Buscar un elemento en el árbol
surfTree :: (Ord a) => a -> ArbolBin a -> Bool
surfTree _ Vacio = False  -- No encontrado en un árbol vacío
surfTree x (Nodo v izq der)
    | x == v    = True  -- Encontrado
    | x < v     = surfTree x izq  -- Buscar en el subárbol izquierdo
    | otherwise = surfTree x der  -- Buscar en el subárbol derecho

-- Recorrer el árbol en orden (izquierda, raíz, derecha)
inOrderTree :: (Ord a) => ArbolBin a -> [a]
inOrderTree Vacio = []
inOrderTree (Nodo v izq der) = inOrderTree izq ++ [v] ++ inOrderTree der

-- Recorrido Preorden (raíz, izquierda, derecha)
preOrderTree :: (Ord a) => ArbolBin a -> [a]
preOrderTree Vacio = []
preOrderTree (Nodo v izq der) = [v] ++ preOrderTree izq ++ preOrderTree der

-- Recorrido Postorden (izquierda, derecha, raíz)
postOrderTree :: (Ord a) => ArbolBin a -> [a]
postOrderTree Vacio = []
postOrderTree (Nodo v izq der) = postOrderTree izq ++ postOrderTree der ++ [v]