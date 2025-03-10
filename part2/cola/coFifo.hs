-- cola como FIFO:

newtype ColaPrio a = CP [a] deriving (Show, Eq)

-- Crea una cola de prioridad vacía
mkqpr :: ColaPrio a
mkqpr = CP []

-- Agrega un elemento manteniendo la lista ordenada (menor primero)
addqpr :: (Ord a) => a -> ColaPrio a -> ColaPrio a
addqpr x (CP []) = CP [x]  
addqpr v (CP a@(x:xs))
                  |v < x = CP (v:a)
                  |v > x = let (CP zs) = addqpr v (CP xs)  in CP (x:zs)
                  |otherwise = CP a


-- Devuelve el elemento con mayor prioridad (el menor en este caso)
nextqpr :: ColaPrio a -> a
nextqpr (CP []) = error "Cola vacía"
nextqpr (CP (x:_)) = x

-- Elimina el elemento con mayor prioridad
popqpr :: ColaPrio a -> ColaPrio a
popqpr (CP [])= CP []
popqpr (CP (_:xs)) = CP xs