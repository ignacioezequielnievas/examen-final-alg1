module Set (Set, emptySet, setEmpty, inSet, addSet, delSet, unionSet) where

data Set a = Empty | Nodo a (Set a) (Set a) deriving (Show,Eq)

emptySet :: Set a
emptySet= Empty

setEmpty :: Set a -> Bool
setEmpty Empty  = True
setEmpty _ = False

inSet :: (Ord a) => a -> Set a -> Bool
inSet _ Empty = False
inSet x (Nodo v izq der)
                    |x < v = inSet x izq
                    |x > v = inSet x der
                    |otherwise = True

addSet :: (Ord a) => a -> Set a -> Set a
addSet x Empty = Nodo x Empty Empty
addSet x (Nodo v izq der)
                    |x < v =Nodo v (addSet x izq) der
                    |x > v =Nodo v izq  (addSet x der)
                    |otherwise =Nodo v izq der

delSet :: (Ord a) => a -> Set a -> Set a
delSet _ Empty = Empty
delSet x (Nodo v izq der)
                    |x < v =Nodo v (delSet x izq) der
                    |x > v =Nodo v izq  (delSet x der)
                    |otherwise = borrarNodo (Nodo v izq der)

borrarNodo:: (Ord a) => Set a -> Set a
borrarNodo (Nodo _ Empty der) = der
borrarNodo (Nodo _ izq Empty) = izq
borrarNodo (Nodo _ izq der) = Nodo vMin izq (delSet vMin der)
                                    where vMin = lowSet der
lowSet::(Ord a) => Set a -> a
lowSet Empty = error "arbol vacio"
lowSet (Nodo x Empty _) = x
lowSet(Nodo _ izq _)= lowSet izq

unionSet :: (Ord a) => Set a -> Set a -> Set a
unionSet s1 Empty = s1
unionSet Empty s2 = s2
unionSet (Nodo v izq der) s2 = addSet v(unionSet izq( unionSet der  s2))