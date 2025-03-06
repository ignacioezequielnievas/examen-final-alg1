-- cola como arbol:

-- Definición del tipo de datos
data ColaPrioridad a = Vacio | Nodo a (ColaPrioridad a) (ColaPrioridad a)
    deriving (Show, Eq)

-- Crear una cola de prioridad vacía
mkpqr :: ColaPrioridad a
mkpqr = Vacio

-- Insertar un elemento en la cola de prioridad
addpqr :: (Ord a) => a -> ColaPrioridad a -> ColaPrioridad a
addpqr x Vacio = Nodo x Vacio Vacio
addpqr x (Nodo y izq der)
    | x < y     = Nodo y (addpqr x izq) der  -- Insertar en el subárbol izquierdo
    | x > y     = Nodo y izq (addpqr x der)  -- Insertar en el subárbol derecho
    | otherwise = Nodo y izq der  -- Si es igual, no se inserta (evita duplicados)


-- Obtener el elemento con la clave más baja (mínimo)
nextqpr :: (Ord a) => ColaPrioridad a -> a
nextqpr Vacio = error "La cola de prioridad está vacía"
nextqpr (Nodo x Vacio _) = x   -- El más a la izquierda es el mínimo
nextqpr (Nodo _ izq _) = nextqpr izq  -- Buscar en el subárbol izquierdo

-- Eliminar el elemento con la clave más baja
poppqr :: (Ord a) => ColaPrioridad a -> ColaPrioridad a
poppqr Vacio = Vacio
poppqr (Nodo _ Vacio der) = der  -- Si el mínimo está en la raíz, lo eliminamos
poppqr (Nodo x izq der) = Nodo x (poppqr izq) der  -- Recursión hasta encontrar el mínimo