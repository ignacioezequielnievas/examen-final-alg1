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
    | otherwise = Nodo y izq (addpqr x der)  -- Insertar en el subárbol derecho

-- Obtener el elemento con la clave más baja (mínimo)
nextpqr :: ColaPrioridad a -> Maybe a
nextpqr Vacio = Nothing
nextpqr (Nodo x Vacio _) = Just x  -- El nodo más a la izquierda tiene la clave más baja
nextpqr (Nodo _ izq _) = nextpqr izq

-- Eliminar el elemento con la clave más baja
poppqr :: (Ord a) => ColaPrioridad a -> ColaPrioridad a
poppqr Vacio = Vacio
poppqr (Nodo _ Vacio der) = der  -- Si el mínimo está en la raíz, lo eliminamos
poppqr (Nodo x izq der) = Nodo x (poppqr izq) der  -- Recursión hasta encontrar el mínimo