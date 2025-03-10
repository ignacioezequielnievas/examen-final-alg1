{-extra-}
----------------------------------------------------------
split::[a]->([a],[a])
split [] = ([],[])
split [x] = ([x],[])
split (x:y:xs) = (x:l1,y:l2)
                        where (l1,l2) = split xs

merge :: (Ord a) => [a]->[a]->[a]
merge a [] = a
merge [] b  = b
merge a@(x:xs) b@(y:ys)
                    |x <= y = x: merge xs b
                    |otherwise = y : merge a ys

msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort l = let 
             (a,b) = split l
             d= msort a
             c = msort b
           in merge d c
{-
  cola prioridad FIFO
  mkqpr = Crea una cola de prioridad vacía
  addqpr =Agrega un elemento manteniendo la lista ordenada (menor primero)
  nextqpr= Devuelve el elemento con mayor prioridad (el menor en este caso)
  popqpr= Elimina el elemento con mayor prioridad
  -}

newtype Cp a  = Cp [a] deriving (Show,Eq)

mkqpr:: Cp a
mkqpr = Cp[]

addqpr::(Ord a) => a -> Cp a -> Cp a
addqpr x (Cp []) = Cp[x]
addqpr v (Cp a@(x:xs))
                |v < x = Cp(v:a)
                |v > x = let (Cp zs) = addqpr v (Cp xs) in Cp (x:zs)
                |otherwise = Cp a

nextqpr::(Ord a) => Cp a -> a
nextqpr (Cp []) = error "cola vacia"
nextqpr (Cp (x:_)) = x

popqpr::(Ord a) => Cp a -> Cp a
popqpr(Cp []) = Cp []
popqpr (Cp (_:xs)) =Cp xs

   {-
  set como arbol binario
  module Set (Set, emptySet, setEmpty, inSet, addSet, delSet, unionSet) where
  emptySet :: Set a
  setEmpty :: Set a -> Bool
  inSet :: (Ord a) => a -> Set a -> Bool
  addSet :: (Ord a) => a -> Set a -> Set a
  delSet :: (Ord a) => a -> Set a -> Set a
  unionSet :: (Ord a) => Set a -> Set a -> Set a
  sigue en e42
  -}