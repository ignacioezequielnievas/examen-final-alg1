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

-- Recorrer el árbol en orden (izquierda, raíz, derecha)
inOrderTree :: (Ord a) => ArbolBin a -> [a]
inOrderTree Vacio = []
inOrderTree (Nodo v izq der) = inOrderTree izq ++ [v] ++ inOrderTree der


-- Buscar un elemento en el árbol
surfTree :: (Ord a) => a -> ArbolBin a -> Bool
surfTree _ Vacio = False  -- No encontrado en un árbol vacío
surfTree x (Nodo v izq der)
    | x == v    = True  -- Encontrado
    | x < v     = surfTree x izq  -- Buscar en el subárbol izquierdo
    | otherwise = surfTree x der  -- Buscar en el subárbol derecho

-- Obtener el elemento con la clave más baja (mínimo)
lowTree :: (Ord a) => ArbolBin a -> a
lowTree Vacio = error "La cola de prioridad está vacía"
lowTree (Nodo x Vacio _) = x   -- El más a la izquierda es el mínimo
lowTree (Nodo _ izq _) = lowTree izq  -- Buscar en el subárbol izquierdo

-- Obtener el elemento con la clave más alta (maximo)
highTree :: (Ord a) => ArbolBin a -> a
highTree Vacio = error "La cola de prioridad está vacía"
highTree (Nodo x _ Vacio ) = x   -- El más a la derecho es el mínimo
highTree (Nodo _ _ der) = highTree der -- Buscar en el subárbol derecho


-- Recorrido Preorden (raíz, izquierda, derecha)
preOrderTree :: (Ord a) => ArbolBin a -> [a]
preOrderTree Vacio = []
preOrderTree (Nodo v izq der) = [v] ++ preOrderTree izq ++ preOrderTree der

-- Recorrido Postorden (izquierda, derecha, raíz)
postOrderTree :: (Ord a) => ArbolBin a -> [a]
postOrderTree Vacio = []
postOrderTree (Nodo v izq der) = postOrderTree izq ++ postOrderTree der ++ [v]

-- Eliminar el elemento con la clave más baja
popLow :: (Ord a) => ArbolBin a -> ArbolBin a
popLow Empty = Empty
popLow (Nodo _ Empty der) = der  -- Si el mínimo está en la raíz, lo eliminamos
popLow (Nodo x izq der) = Nodo x (popLow izq) der  -- Recursión hasta encontrar el mínimo

-- Eliminar el elemento con la clave más alta
popHigh :: (Ord a) => ArbolBin a -> ArbolBin a
popHigh Empty = Empty
popHigh (Nodo _  izq Empty ) = izq  -- Si el maximo está en la raíz, lo eliminamos
popHigh (Nodo x izq der) = Nodo x izq  (popHigh der)  -- Recursión hasta encontrar el maximo



-- Eliminar un elemento del conjunto (árbol binario)
delTree :: (Ord a) => a -> ArbolBin a -> ArbolBin a
delTree _ Empty = Empty  -- Caso base: árbol vacío, no hay nada que eliminar
delTree x (Nodo v izq der)
    | x < v     = Nodo v (delTree x izq) der  -- Buscar en el subárbol izquierdo
    | x > v     = Nodo v izq (delTree x der)  -- Buscar en el subárbol derecho
    | otherwise = eliminarNodo  (Nodo v izq der)  -- Nodo encontrado, hay que eliminarlo

-- Función auxiliar para eliminar un nodo del árbol
eliminarNodo :: (Ord a) => ArbolBin a -> ArbolBin a
eliminarNodo (Nodo _ Empty der) = der  -- Si no hay hijo izquierdo, devolvemos el derecho
eliminarNodo (Nodo _ izq Empty) = izq  -- Si no hay hijo derecho, devolvemos el izquierdo

eliminarNodo (Nodo _ izq der)   = Nodo minValor izq (delTree minValor der)
            where minValor = lowTree der  -- Reemplazar con el menor elemento del subárbol derecho