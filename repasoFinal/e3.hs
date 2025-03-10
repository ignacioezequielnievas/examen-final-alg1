{- ------------------------------------------------------------------
 Un numero perfecto es aquel que es igual a la suma de sus divisores menores que el, como 6: 3, 2, 1
 Utilizando lista por comprension escribir la funcion perfectosn que de como resultado la lista de 
 numeros perfectos comprendidos por el intervalo [1,n]
-}
sumDivisores::(Integral a) => a ->a
sumDivisores n = sum [y|y<-[1..n-1], mod n y == 0]

perfectosn::(Integral a) => a ->[a]
perfectosn n = [y|y<-[1..n], sumDivisores y == y]

{- 1 -------------------------------------------------------------)
Utilizando ZIP y listas por comprension. Escriba una función que realice el producto escalar de
dos listas. Donde producto escalar estaría definido como la suma de los productos uno a uno,
componente a componente de cada lista. Si una lista tuviera más elementos que la otra, al agotarse
uno de los operandos se detiene la suma.
a1*b1 + a2*b2+...+ai*bi+...+ an*bn.
-}
productoEscalar::(Num a)=>[a]->[a]->a
productoEscalar l1 l2 = sum [(x*y)| (x,y)<- zip l1 l2]

{- 2) -----------------------------------------------------------
Escribir una función que inserta elementos en una lista de manera de mantenerla ordenada de
menor a mayor.
De esta forma cada operación Head sobre la lista devuelve el elemento más chico almacenado en
ella.
Inserta:: (Ord a) => a->[a]->[a]
-}
inserta:: (Ord a) => a->[a]->[a]
inserta x []= [x]
inserta v a@(x:xs)
            |v <= x = (v:a)
            |otherwise = let zs = inserta v xs in (x:zs)
           


{- 3) ------------------------------------------------------
Defina un tipo de dato árbol binario de búsqueda (ArbolBin) 
Escriba el método addTree e inOrderTree.
addTree :: (Ord a) => a -> ArbolBin a -> ArbolBin a
Inserta un elemento del tipo a en un arbol binario.
inOrderTree :: (Ord a)=> ArbolBin a -> [a]
Produce un listado “En Orden” del árbol binario.
El listado en orden del árbol se define de la siguiente manera, primero se lista en orden el árbol
izquierdo, luego la raíz y finalmente se lista en orden el árbol derecho.

y 5 mas :
emptyTree ::ArbolBin a 
inTree::(Ord a) => a -> ArbolBin a -> Bool
lowTree ::(Ord a) =>ArbolBin a -> a
delTree ::(Ord a) => a -> ArbolBin a -> ArbolBin a
-}

data ArbolBin a = Empty | Nodo a (ArbolBin a) (ArbolBin a) deriving (Show ,Eq)

emptyTree ::ArbolBin a 
emptyTree = Empty

addTree :: (Ord a) => a -> ArbolBin a -> ArbolBin a
addTree x Empty = Nodo x Empty Empty
addTree x (Nodo v izq der)
                    |x < v = Nodo v (addTree x izq) der
                    |x > v = Nodo v izq (addTree x der)
                    |otherwise = Nodo v izq der


inTree::(Ord a) => a -> ArbolBin a -> Bool
inTree x Empty = False
inTree x (Nodo v izq der)
                       | x == v = True
                       | x < v = inTree x izq
                       |otherwise = inTree x der 


delTree ::(Ord a) => a -> ArbolBin a -> ArbolBin a
delTree _ Empty = Empty
delTree x (Nodo v izq der)
                    |x < v = Nodo v (delTree x izq) der
                    |x > v = Nodo v izq (delTree x der)
                    |otherwise = borrarNodo (Nodo v izq der)

borrarNodo::(Ord a) =>ArbolBin a -> ArbolBin a
borrarNodo (Nodo _ Empty der) = der
borrarNodo  (Nodo _ izq Empty) = izq
borrarNodo (Nodo _ izq der) = Nodo vMin izq (delTree vMin der)
                                            where vMin = lowTree der

lowTree ::(Ord a) =>ArbolBin a -> a
lowTree Empty = error "arbol vacio"
lowTree (Nodo x Empty _) = x
lowTree (Nodo _ izq _) = lowTree izq

inOrderTree :: (Ord a)=> ArbolBin a -> [a]
inOrderTree Empty = []
inOrderTree (Nodo x izq der) = inOrderTree izq ++ [x] ++inOrderTree der

preOrderTree :: (Ord a)=> ArbolBin a -> [a]
preOrderTree Empty = []
preOrderTree (Nodo x izq der) = [x] ++ preOrderTree izq ++ preOrderTree der

postOrderTree :: (Ord a)=> ArbolBin a -> [a]
postOrderTree Empty = []
postOrderTree (Nodo x izq der) = postOrderTree izq  ++postOrderTree der ++ [x]