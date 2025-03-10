{-
Recordemos que la funcion de biblioteca ZIP recibe como argumento dos listas x:xs e y:ys
y produce una lista de tuplas (i,j) donde los i provienen de la primera lista y los j de 
la segunda.
Cuando una lista es mas larga que la otra, el resultado contempla solo los pares hasta donde pudieron formarse
a) Escriba una version personal de la funcion zip llamada miZip
miZip :: [a] -> [b] -> [(a,b)]
b) Utilizando miZip y listas por comprension escriba una funcion que realice el producto
escalar de dos listas...
c) Utilizando solo la funcion miZip escriba la funcion "indexado"
dada una lista produce una lista de pares donde cada elemento de la lista tiene
su posicion dentro de la misma comenzando en 1
-}
miZip :: [a] -> [b] -> [(a,b)]
miZip [] _ = []
miZip _ [] = []
miZip (x:xs) (y:ys) = (x,y) : miZip xs ys


productoEscalar::(Num a)=>[a]->[a]->a
productoEscalar l1 l2 = sum [(x*y)| (x,y)<- miZip l1 l2]

indexado::[a]->[(a,Int)]
indexado l = miZip l [1..]

{-
    Consideremos la siguiente funcion 
    split :: (Ord a) => a -> [a] -> ([a], [a])
    split x l = ([y | y <- l, y <= x], [y | y <- l, y > x])
    defina una version de esta funcion que trabaje en exactamente una sola pasada
    a la lista l
-}
split :: (Ord a) => a -> [a] -> ([a], [a])
split _ [] =([],[])
split p (x:xs)
            |x <= p = (x:menor, mayor)
            |otherwise = (menor, x:mayor)
                where (menor,mayor)= split p xs


{-
 Un conjunto o Set es una coleccion de items del mismo tipo distinguibles entre
 si por su clave o valor en el cual un item puede ser testeado si es 
 miembro, insertado o borrado de la coleccion 
 La cantidad de elementos distintos es lo que se denomina tamaño del conjunto
 module Set (Set,  emptySet, setEmpty, inSet, addSet, delSet, unionSet) where
 emptySet :: Set a
 setEmpty :: Set a -> Bool
 inSet :: (Eq a) => a -> Set a -> Bool
 addSet :: (Eq a) => a -> Set a -> Set a
 delSet :: (Eq a) => a -> Set a -> Set a
 unionSet :: (Eq a) => Set a -> Set a -> Set a
 
 Defina el tipo de dato e implemente los metodos del nuevo tipo de dato
 utilizando listas no ordenadas y sin duplicados.
 El metodo unionSet (union de los dos conjuntos) se escribira haciendo uso 
 de los metodos ya definidos, es decir, sin operar directamente la lista sino
 que directamente se operara el SET
 sigue en e22
 -}

