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
sum'::(Num a)=>[a] ->a
sum' [] = 0
sum' (x:xs) = x + sum' xs

miZip :: [a] -> [b] -> [(a,b)]
miZip [] _ = []
miZip _ []= []
miZip (x:xs) (y:ys) = (x,y) : miZip xs ys

productoEscalar::(Num a)=>[a]->[a]->a
productoEscalar l1 l2 = sum' [(x*y)|(x,y) <- miZip l1 l2]

indexado::[a]->[(a,Int)]
indexado a = miZip a [1..]

{-
    Consideremos la siguiente funcion 
    split :: (Ord a) => a -> [a] -> ([a], [a])
    split x l = ([y | y <- l, y <= x], [y | y <- l, y > x])
    defina una version de esta funcion que trabaje en exactamente una sola pasada
    a la lista l
-}

splits :: (Ord a) => a -> [a] -> ([a], [a])
splits _ [] = ([],[])
splits p (x:xs) 
            |x <= p = (x:menor,mayor)
            |otherwise = (menor, x:mayor)
                where (menor,mayor) = splits p xs

{- ------------------------------------------------------------------
 Un numero perfecto es aquel que es igual a la suma de sus divisores menores que el, como 6: 3, 2, 1
 Utilizando lista por comprension escribir la funcion perfectosn que de como resultado la lista de 
 numeros perfectos comprendidos por el intervalo [1,n]
-}
sumDivisores::(Integral a )=> a ->a
sumDivisores n = sum'[y|y<-[1..n-1], mod n y == 0]

perfectosn::(Integral a )=> a ->[a]
perfectosn n = [y|y<-[1..n], sumDivisores y == y]

{- 1 -------------------------------------------------------------)
Utilizando ZIP y listas por comprension. Escriba una función que realice el producto escalar de
dos listas. Donde producto escalar estaría definido como la suma de los productos uno a uno,
componente a componente de cada lista. Si una lista tuviera más elementos que la otra, al agotarse
uno de los operandos se detiene la suma.
a1*b1 + a2*b2+...+ai*bi+...+ an*bn.
-}
produEscalar::(Num a)=>[a]->[a]->a
produEscalar l1 l2 = sum' [(x*y)|(x,y) <- zip l1 l2]

{- 2) -----------------------------------------------------------
Escribir una función que inserta elementos en una lista de manera de mantenerla ordenada de
menor a mayor.
De esta forma cada operación Head sobre la lista devuelve el elemento más chico almacenado en
ella.
Inserta:: (Ord a) => a->[a]->[a]
-}
inserta:: (Ord a) => a->[a]->[a]
inserta x [] = [x]
inserta v a@(x:xs)
            |v <= x = (v:a)
            |otherwise =  let zs = inserta v xs in (x:zs)

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
miZip' :: [a] -> [b] -> [(a,b)]
miZip' [] _ = []
miZip' _ []= []
miZip' (x:xs) (y:ys) = (x,y) : miZip' xs ys

indexado'::[a]->[(a,Int)]
indexado' a = miZip a [1..]

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
                    |x <= y = x: juntar xs b
                    |otherwise = y : juntar a ys

particion :: Ord a => a -> [a] -> ([a], [a])
particion _ [] = ([], [])
particion p (x:xs)
                | x <= p =(x:menor, mayor)
                |otherwise =(menor, x:mayor)
                    where (menor, mayor) = particion p xs

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (p:xs) = qsort menor ++[p] ++qsort mayor
                         where (menor, mayor) = particion p xs
{-extra-}
----------------------------------------------------------
split::[a]->([a],[a])
split [] = ([], [])
split [x] = ([x], [])
split (x:y:xs) = (x:l1,y:l2)
                    where (l1,l2) = split xs

merge :: (Ord a) => [a]->[a]->[a]
merge a [] = a
merge [] b =b
merge a@(x:xs) b@(y:ys)
                    |x<= y = x: merge xs b
                    |otherwise = y : merge a ys

-- Implementación de Merge Sort
msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort l  = let 
                (a,b) = split l
                d = msort a
                c = msort b
            in merge d c