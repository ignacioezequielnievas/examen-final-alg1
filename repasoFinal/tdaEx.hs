{-
 Un conjunto o Set es una coleccion de items del mismo tipo distinguibles entre
 si por su clave o valor en el cual un item puede ser testeado si es 
 miembro, insertado o borrado de la coleccion 
 La cantidad de elementos distintos es lo que se denomina tamaño del conjunto
 module Set (Set,  emptySet, setEmpty, inSet, addSet, delSet) where
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
 -}
module Set (Set,  emptySet, setEmpty, inSet, addSet, delSet) where

newtype Set a = Set [a] deriving (Show,Eq)

emptySet :: Set a
emptySet = Set []

setEmpty :: Set a -> Bool
setEmpty (Set []) = True
setEmpty _ = False



inSet :: (Eq a) => a -> Set a -> Bool
inSet _ (Set []) = False
inSet v (Set (x:xs))
                | v == x = True
                |otherwise = inSet v (Set xs)

addSet :: (Eq a) => a -> Set a -> Set a
addSet x (Set []) = Set [x]
addSet v  a@(Set xs) 
                  | inSet v  a =  a
                  |otherwise = Set (v:xs)

   

delSet :: (Eq a) => a -> Set a -> Set a
delSet _ (Set []) = Set []
delSet v (Set (x:xs))
                |x == v = Set xs
                |otherwise = let (Set zs)=delSet x (Set xs) in Set (x:zs) 


unionSet :: (Eq a) => Set a -> Set a -> Set a
unionSet (Set []) s2 = s2
unionSet s1 (Set []) = s1
unionSet (Set (x:xs))  s2 = unionSet (Set xs) (addSet x s2)


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

data  ColaPrioridad a = Empty | Nodo a (ColaPrioridad) (ColaPrioridad a) deriving (Show,Eq)

mkqpr::ColaPrioridad a
mkqpr = Empty


addqpr::(Ord a)=> a-> ColaPrioridad a -> ColaPrioridad a
addqpr x Empty = Nodo x Empty Empty
addqpr x (Nodo v izq der)
                    |x < v = Nodo v (addqpr x izq) der
                    |x > v = Nodo v izq (addqpr x der)
                    |otherwise = Nodo v izq der

nextqpr::(Ord a)->ColaPrioridad a ->a
nextqpr Empty = error "cola vacia"
nextqpr (Nodo x Empty _) = x
nextqpr (Nodo _izq _) = nextqpr izq



popqpr::(Ord a) -> ColaPrioridad a -> ColaPrioridad a
popqpr Empty = Empty
popqpr (Nodo _ Empty der ) = der
popqpr(Nodo x izq der ) = Nodo x (popqpr izq ) der



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
elemTree::(Ord a) => a -> ArbolBin a -> Bool
delTree ::(Ord a) => a -> ArbolBin a -> ArbolBin a
-}
data ArbolBin a = Empty | Nodo a (ArbolBin a) (ArbolBin a) deriving (Show,Eq)

emptyTree::ArbolBin a
emptyTree = Empty

addTree :: (Ord a) => a -> ArbolBin a -> ArbolBin a
addTree x Empty = Nodo x Empty Empty
addTree x (Nodo v izq der)
                      |x < v = Nodo v (addTree x izq) der
                      |x > v = Nodo v izq (addTree x der)
                      |otherwise = Nodo v izq der

inOrderTree :: (Ord a)=> ArbolBin a -> [a]
inOrderTree Empty = []
inOrderTree (Nodo v izq der) = inOrderTree izq ++ [v] ++ inOrderTree der

preOrderTree :: (Ord a)=> ArbolBin a -> [a]
preOrderTree Empty = []
preOrderTree (Nodo v izq der)= [v] ++ inOrderTree izq  ++ inOrderTree der

postOrderTree :: (Ord a)=> ArbolBin a -> [a]
postOrderTree Empty = []
postOrderTree (Nodo v izq der)= inOrderTree izq  ++ inOrderTree der ++ [v]

inTree::(Ord a) => a -> ArbolBin a -> Bool
inTree _ Empty = False
inTree x (Nodo v izq der)
                    |x == v = True
                    |x < v = inTree x izq
                    |otherwise = inTree x der

delTree ::(Ord a) => a -> ArbolBin a -> ArbolBin a
delTree _ Empty =  Empty 
delTree x (Nodo v izq der)
                      |x < v = Nodo v (delTree x izq) der
                      |x > v = Nodo v izq (delTree x der)
                      |otherwise = borrarNodo (Nodo v izq der)



borrarNodo::(Ord a)=>ArbolBin a -> ArbolBin a
borrarNodo (Nodo _ Empty der) = der
borrarNodo (Nodo _  izq Empty ) = izq
borrarNodo (Nodo _  izq der ) = Nodo vMin izq (delTree vMin der)
                                      where vMin = lowTree der

lowTree ::(Ord a) =>ArbolBin a -> a
lowTree Empty = error "arbol vacio"
lowTree (Nodo x  Empty _ ) = x
lowTree (Nodo _  izq  _ ) = lowTree izq

{-Extras -}
{-
  cola prioridad FIFO
  mkqpr = Crea una cola de prioridad vacía
  addqpr =Agrega un elemento manteniendo la lista ordenada (menor primero)
  nextqpr= Devuelve el elemento con mayor prioridad (el menor en este caso)
  popqpr= Elimina el elemento con mayor prioridad
  -}

newtype Cp a = Cp [a] deriving (Show , Eq)

mkqpr::Cp a
mkqpr = Cp []

addqpr::(Ord a)=> a ->Cp a-> Cp a
addqpr x (Cp []) = Cp [x]
addqpr v (Cp a@(x:xs))
                  |v < x = Cp (v:a)
                  |v > x = let (Cp zs) = addqpr v (Cp xs)  in Cp (x:zs)
                  |otherwise = Cp a

nextqpr::(Ord a)=>Cp a->a
nextqpr (Cp []) = error "cola vacia"
nextqpr (Cp (x:_))= x
                



popqpr::(Ord a)=>Cp a->Cp a
popqpr (Cp []) = Cp []
popqpr (Cp (_:xs))= xs


  {-
  set como arbol binario
  module Set (Set, emptySet, setEmpty, inSet, addSet, delSet, unionSet) where
  emptySet :: Set a
  setEmpty :: Set a -> Bool
  inSet :: (Ord a) => a -> Set a -> Bool
  addSet :: (Ord a) => a -> Set a -> Set a
  delSet :: (Ord a) => a -> Set a -> Set a
  unionSet :: (Ord a) => Set a -> Set a -> Set a
  -}
module Set (Set, emptySet, setEmpty, inSet, addSet, delSet, unionSet) where

data Set a = Empty | Nodo a (Set a) (Set a)

emptySet :: Set a
emptySet = Empty

setEmpty :: Set a -> Bool
setEmpty Empty = True
setEmpty _ = False

inSet :: (Ord a) => a -> Set a -> Bool
inSet x Empty = False
inSet x (Nodo v izq der)
                    | x == v = True
                    | x < v =inSet x izq
                    |otherwise = inSet x der

addSet :: (Ord a) => a -> Set a -> Set a
addSet x Empty = Nodo x Empty Empty
addSet x (Nodo v izq der )
                        |x < v = Nodo v (addqpr x izq) der
                        |x > v = Nodo v izq (addqpr x der)
                        |otherwise = Nodo v izq der


delSet :: (Ord a) => a -> Set a -> Set a
delSet _ Empty = Empty
delSet x (Nodo v izq der )
                     |x < v = Nodo v (delqpr x izq) der
                     |x > v = Nodo v izq (delqpr x der)
                     |otherwise = borrarNodoSet (Nodo v izq der )
                    
borrarNodoSet :: (Ord a) =>Set a -> Set a
borrarNodoSet (Nodo _ Empty der ) = der
borrarNodoSet (Nodo _ izq Empty )  = izq
borrarNodoSet (Nodo _ izq der ) = Nodo vMin izq (delTree Vmin der)
                                      where vMin = lowTreeSet der


lowTreeSet:: (Ord a) => Set a -> a 
lowTreeSet Empty = error "arbol vacio"
lowTreeSet (Nodo x Empty _ ) = x
lowTreeSet (Nodo _ izq _ ) = lowTreeSet izq 


  {-
  -- con listas ordenadas
 module Set (Set,  emptySet, setEmpty, inSet, addSet, delSet) where
 emptySet :: Set a
 setEmpty :: Set a -> Bool
 inSet :: (Eq a) => a -> Set a -> Bool
 addSet :: (Eq a) => a -> Set a -> Set a
 delSet :: (Eq a) => a -> Set a -> Set a
 unionSet :: (Eq a) => Set a -> Set a -> Set a
  -}
module Set (Set,  emptySet, setEmpty, inSet, addSet, delSet) where

newtype Set a = Set [a] deriving (Show ,Eq)

emptySet :: Set a
emptySet = Set []

setEmpty :: Set a -> Bool
setEmpty (Set [] ) = True
setEmpty _ = False

inSet :: (Eq a) => a -> Set a -> Bool
inSet _ (Set []) = False
inSet v (Set (x:xs))
                   |v == x = True
                   |otherwise = inSet v (Set xs)
                    

addSet :: (Eq a) => a -> Set a -> Set a
addSet x (Set []) = Set [x]
addSet v (Set a@(x:xs))
                    |v < x = Set (v:a)
                    |v > x = let (Set zs) = addSet v (Set xs) in Set (x:zs)
                    |otherwise = Set a

delSet :: (Eq a) => a -> Set a -> Set a
delSet _ (Set []) = Set []
delSet v (Set a@(x:xs))
                    |v < x = Set a
                    |v > x = let (Set zs) = delSet v (Set xs) in Set (x:zs)
                    |otherwise = Set xs

unionSet :: (Eq a) => Set a -> Set a -> Set a
unionSet s1 (Set []) = s1
unionSet (Set []) s2 = s2
unionSet a@(Set (x:xs)) b@(Set (y:ys))
                                  |x == y = let (Set zs) = unionSet (Set xs) (Set ys) in Set (x:zs)
                                  |x < y =let (Set zs) = unionSet (Set xs) b in Set (x:zs)
                                  |otherwise = let (Set zs) = unionSet a (Set ys) in Set (y:zs)


{-pila Lifo
module Pila (Stack, pop, push, top, emptyStk, stackIsEmpty) where 
emptyStk:: Stack a 
push:: a -> Stack a -> Stack a
pop :: Stack a -> Stack a 
top:: Stack a -> a 
stackIsEmpty :: Stack a -> Bool 
-}

module Pila (Stack, pop, push, top, emptyStk, stackIsEmpty) where 

newtype Stack a = Stack [a] deriving (Show)

emptyStk:: Stack a 
emptyStk = Stack []


push:: a -> Stack a -> Stack a
push x (Stack []) = Stack [x]
push v (Stack a@(x:xs) ) = Stack (v:a)


pop :: Stack a -> Stack a 
pop (Stack []) = error "pila vacia"
pop (Stack (_:xs)) = Stack xs


top:: Stack a -> a 
top (Stack []) = error "pila vacia"
top (Stack (x:_)) = x

stackIsEmpty :: Stack a -> Bool 
stackIsEmpty (Stack []) = True
stackIsEmpty _ = False