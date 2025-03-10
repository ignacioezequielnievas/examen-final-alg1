{- final 2024:
Recordemos que la funcion de biblioteca ZIP recibe como argumento dos listas x:xs e y:ys
y produce una lista de tuplas (i,j) donde los i provienen de la primera lista y los j de 
la segunda.
Cuando una lista es mas larga que la otra, el resultado contempla solo los pares hasta donde pudieron formarse
a) Escriba una version personal de la funcion zip llamada miZip
miZip :: [a] -> [b] -> [(a,b)]
b) Utilizando solo la funcion miZip escriba la funcion "indexado"
dada una lista produce una lista de pares donde cada elemento de la lista tiene
su posicion dentro de la misma comenzando en 1
-}
miZip :: [a] -> [b] -> [(a,b)]
miZip [] _ = []
miZip _ [] = []
miZip (x:xs)  (y:ys) = (x,y) : miZip xs ys

indexado::[a] ->[(a,Int)]
indexado l = miZip l [1..]

{- final 2024:
a)
Escribir una funcion que recibe como argumento dos listas ordenadas y devuleve una 
lista ordenada fusión de las listas argumentos 
(No se debe usar ningun metodo de clasificacion)
juntar :: (Ord a) => [a] -> [a] ->  [a]

 b)Escriba una funcion Qsort :: (Ord a) => [a] -> [a] sin utilizar listas por comprension
 Nota: Escriba una funcion particion que reciba como argumento un valor de referencia o pivot
 y una lista de valores del mismo tipo que el pivot
 Esta funcion da como resultado una tupla con dos listas (l1, l2) de modo que en l1 estan todos los valores
 que son menores o iguales que el pivot y en l2 todos los mayores que el pivot
 particion :: Ord a => a -> [a] -> ([a], [a])      

-}
juntar :: (Ord a) => [a] -> [a] ->  [a]
juntar a [] = a
juntar [] b = b
juntar a@(x:xs) b@(y:ys)
                    |x <= y = x :juntar xs b
                    |otherwise = y : juntar a ys

particion :: Ord a => a -> [a] -> ([a], [a])
particion _ [] = ([],[])
particion p (x:xs)
                | x <= p = (x:menor,mayor)
                |otherwise = (menor, x:mayor)
                    where (menor,mayor) = particion p xs
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort (x:xs) = qsort menor ++[x] ++ qsort mayor
                where (menor,mayor) = particion x xs

 {-
Una cola de prioridad es una estructura de datos que almacena elementos "Clasificables"
Con la particularidad de que cuando se saca uno de ella siempre se extrae el elemento con menor clave
de ahi su nombre pues clasifica los elementos en funcion de su prioridad mas baja primero
Las funciones que manipulan a la cola de prioridad son:
mkqpr: instancia una nueva cola de prioridad vacia
addqpr: agrega un nuevo elemento
nextqpr: devuelve el elemento con clave mas baja
popqpr: devuelve una cola de prioridad donde se quito el nextqpr
defina el tad ColaPrioridad e implemente el mismo utilizando un arbol binario de busqueda
como estructura de almacenamiento
Escribir todas las funciones necesarias para la manipulacion de la estructura subyacente es decir para
manipular el arbol
sugerencia: recordar como extraer el elemento con clave mas pequeña de un arbol
-}

data ColaPrioridad a = Empty | Nodo a (ColaPrioridad a) (ColaPrioridad a) deriving (Show , Eq)

mkqpr::ColaPrioridad a
mkqpr = Empty

addqpr ::(Ord a)=> a ->ColaPrioridad a -> ColaPrioridad a
addqpr x Empty = Nodo x Empty Empty
addqpr v (Nodo x izq der) 
                        |v < x = Nodo x (addqpr v izq) der
                        |v > x= Nodo x izq (addqpr v der)
                        |otherwise= Nodo x izq der

nextqpr::(Ord a)=> ColaPrioridad a -> a
nextqpr Empty = error "arbol vacio"
nextqpr (Nodo x Empty _)= x 
nextqpr (Nodo _ izq _) = nextqpr izq

popqpr::(Ord a)=>ColaPrioridad a -> ColaPrioridad a
popqpr Empty = Empty
popqpr (Nodo _ Empty der) = der
popqpr (Nodo x izq der ) = Nodo x (popqpr izq) der