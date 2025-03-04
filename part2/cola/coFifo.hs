-- cola como FIFO:

newtype ColaPrio a = CP [a] deriving (Show, Eq)

-- Crea una cola de prioridad vacía
mkqpr :: ColaPrio a
mkqpr = CP []

-- Agrega un elemento manteniendo la lista ordenada (menor primero)
addqpr :: (Ord a) => a -> ColaPrio a -> ColaPrio a
addqpr x (CP []) = CP [x]
addqpr x (CP xs) = CP (insertarOrdenado x xs)

-- Inserta un elemento en la posición correcta para mantener el orden
insertarOrdenado :: (Ord a) => a -> [a] -> [a]
insertarOrdenado x [] = [x]
insertarOrdenado x (y:ys)
  | x <= y    = x : y : ys
  | otherwise = y : insertarOrdenado x ys

-- Devuelve el elemento con mayor prioridad (el menor en este caso)
nextqpr :: ColaPrio a -> a
nextqpr (CP []) = error "Cola vacía"
nextqpr (CP (x:_)) = x

-- Elimina el elemento con mayor prioridad
popqpr :: ColaPrio a -> ColaPrio a
popqpr (CP []) = error "Cola vacía"
popqpr (CP (_:xs)) = CP xs