sum'::(Num a) => [a]->a
sum' [] = 0
sum' (x:xs) = x + sum' xs 

sumaDivisores ::(Integral a)=> a->a
sumaDivisores d = sum' [x | x <- [1..d], mod d x == 0]

perfectos::(Integral a)=> a->[a]
perfectos p = [x | x <- [1..p], sumaDivisores x == x]

----------------------------------------------------------

juntar :: (Ord a) => [a]->[a]->[a]
juntar [] ys =ys
juntar xs [] = xs
juntar (x:xs) (y:ys) = if x <= y 
      then x : juntar xs (y:ys)
      else  y : juntar (x:xs) ys

particion :: Ord a => a ->[a]->([a],[a])
particion _ [] = ([],[])
particion p (x:xs) = if x < p
     then  (x:menor,mayor)
     else (menor,x:mayor)
          where (menor,mayor) = particion p xs

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort [a] = [a]
qsort (p:xs) = qsort menor++[p]++ qsort mayor
              where (menor, mayor) = particion p xs 

----------------------------------------------------------

miZip :: [a] -> [b] -> [(a,b)]
miZip [] _ = []
miZip _ [] = []
miZip (x:xs) (y:ys) = (x,y):miZip xs ys

indexado :: [a] -> [(a, Int)]
indexado lista = miZip lista [1..]

----------------------------------------------------------

prodEscalar :: [Int] -> [Int] -> Int
prodEscalar [] _ = 0
prodEscalar _ [] = 0
prodEscalar (x:xs) (y:ys) = (x*y) + prodEscalar xs ys 

----------------------------------------------------------

inserta :: (Ord a) => a -> [a] -> [a]
inserta a [] = [a]
inserta a (x:xs) = if a < x then a:x:xs else x:inserta a xs

----------------------------------------------------------
split::[a]->([a],[a])
split [] = ([],[])
split [x] = ([x],[])
split (x:y:zs) = (x:l1,xs:l2)
    where (l1,l2) = split zs