module Pila (Stack, pop, push, top, emptyStk, stackIsEmpty) where 

data Stack a = EmptyStk | Stk a (Stack a)  deriving (Show)

emptyStk:: Stack a 
emptyStk = EmptyStk 

push:: a -> Stack a -> Stack a 
push x s = Stk x s 

pop          :: Stack a -> Stack a 
pop EmptyStk = error "Stack Vacio" 
pop (Stk _ s)  = s 

top          :: Stack a -> a 
top EmptyStk = error "Stack Vacio" 
top (Stk x _) = x 

stackIsEmpty :: Stack a -> Bool 
stackIsEmpty EmptyStk = True 
stackIsEmpty _ = False 

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
