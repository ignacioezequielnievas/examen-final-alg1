  {-
  -- con listas ordenadas
 module Set (Set,  emptySet, setEmpty, inSet, addSet, delSet) where
 emptySet :: Set a
 setEmpty :: Set a -> Bool
 inSet :: (Eq a) => a -> Set a -> Bool
 addSet :: (Eq a) => a -> Set a -> Set a
 delSet :: (Eq a) => a -> Set a -> Set a
 unionSet :: (Eq a) => Set a -> Set a -> Set a
 continua en e52
  -}

  
{-pila Lifo
module Pila (Stack, pop, push, top, emptyStk, stackIsEmpty) where 
emptyStk:: Stack a 
push:: a -> Stack a -> Stack a
pop :: Stack a -> Stack a 
top:: Stack a -> a 
stackIsEmpty :: Stack a -> Bool 
-}
module Pila (Stack, pop, push, top, emptyStk, stackIsEmpty) where 

newtype Stack a  = Stack [a]  deriving (Show)

emptyStk:: Stack a 
emptyStk = Stack []

push:: a -> Stack a -> Stack a
push x (Stack []) = Stack [x]
push v (Stack xs) = Stack (v:xs) 

pop :: Stack a -> Stack a 
pop (Stack []) = error "pila vacia"
pop (Stack (_:xs)) = Stack xs

top:: Stack a -> a
top (Stack []) = error "pila vacia"
top (Stack (x:xs)) = x

stackIsEmpty :: Stack a -> Bool 
stackIsEmpty (Stack []) = True
stackIsEmpty _ = False