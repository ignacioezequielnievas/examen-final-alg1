module Pila (Stack, pop, push, top, emptyStk, stackIsEmpty) where 

newtype Stack a       = Stk [a] deriving (Show )

emptyStk:: Stack a 
emptyStack            = Stk [] 

push:: a -> Stack a -> Stack a 
push x (Stk xs)       = Stk (x:xs) 

pop          :: Stack a -> Stack a 
pop (Stk [])          = error "Pila Vacia" 
pop (Stk (_:xs))      = Stk xs 

top          :: Stack a -> a 
top (Stk [])          = error "Pila Vacia" 
top (Stk (x:_))       = x 


stackIsEmpty :: Stack a -> Bool 
stackIsEmpty (Stk []) = True 
stackIsEmpty (Stk _ ) = False 
