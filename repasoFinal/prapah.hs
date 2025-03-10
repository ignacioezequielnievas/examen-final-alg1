divisores :: (Integral a) => a -> [a]
divisores n = [y|y<-[1..n-1], mod n y == 0]

sum'::(Num a)=> [a]->a
sum' [] = 0 
sum'(x:xs) = x + sum' xs

products::(Num a) => [a]->a
products [] = 1
products (x:xs) = x * products xs

sumDivisores :: (Integral a) => a -> a
sumDivisores n = sum' [y|y<-[1..n-1], mod n y == 0]

perfecto :: (Integral a) => a -> [a]
perfecto n = [y|y<-[1..n], sumDivisores y == y]

ordenMe::(Ord a)=>[a]->[a]
ordenMe [] = []
ordenMe (x:xs) = ordenMe menor ++ [x] ++ ordenMe mayor
                            where menor =[y|y<-xs, y <= x]
                                  mayor =[y|y<-xs, y > x]
ordenMa::(Ord a)=>[a]->[a]
ordenMa [] = []
ordenMa (x:xs) = ordenMa mayor ++ [x] ++ ordenMa menor
                            where mayor =[y|y<-xs, y > x]
                                  menor =[y|y<-xs, y <= x]
                        

insertar :: (Ord a) => a -> [a] -> [a]
insertar x [] = [x]
insertar v a@(x:xs)
               |v <= x = (v:a)
               |otherwise= let zs = insertar v xs in (x:zs)     

length' :: (Num a ) => [a]->a
length' xs = sum' [1|_<-xs]

length'' :: (Num b) => [a] -> b
length'' [] = 0
length''(_:xs) = 1 + length'' xs

miZip :: [a] -> [b] ->[c] ->[(a, b,c)]
miZip [] _ _ = []
miZip _ [] _= []
miZip _  _ []= []
miZip (x:xs) (y:ys) (z:zs) = (x,y,z) : miZip xs ys zs

miZip' :: [a] -> [b] ->[(a, b)]
miZip' [] _ = []
miZip' _ []= []
miZip' (x:xs) (y:ys) = (x,y) : miZip' xs ys

indexado:: [a]-> [(a,Char, Int)]
indexado l = miZip l ['a'..'z'] [1..]

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs


facto:: (Integral a)=> a -> a
facto 0 = 1
facto n = n * facto (n-1)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

takes:: Int ->[a]->[a]
takes _ [] = []
takes 0 l = []
takes n (x:xs) = x: takes (n-1) xs

reverse'::[a]->[a]
reverse' [] = []
reverse' [x] = [x]
reverse'(x:xs) = reverse' xs ++ [x]

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "no hay maximo"
maximum' [x] = x
maximum' (x:xs)
                |x > vMax = x
                |otherwise = vMax
                  where vMax = maximum' xs

minimo::(Ord a) => [a]-> a
minimo [] = error "no hay maximo"
minimo [x] = x
minimo (x:xs)
            |x < vMin = x
            |otherwise = vMin
                  where vMin = minimo xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] =[]
filter' f (x:xs) 
            | f x = x : filter' f xs
            | otherwise = filter' f xs

splits::(Ord a)=> a->[a]->([a],[a])
splits _ [] = ([],[])
splits p (x:xs)
            |x <= p = (x:menor, mayor)
            |otherwise = (menor, x:mayor)
                where (menor,mayor) = splits p xs

split::[a]->([a],[a])
split [] = ([],[])
split [x] = ([x],[])
split (x:y:xs) = (x:l1,y:l2)
                    where (l1,l2) = split xs

merge :: (Ord a) => [a]->[a]->[a]
merge a [] = a
merge [] b = b
merge a@(x:xs) b@(y:ys)
                    |x <= y = x: merge xs b
                    |otherwise = y : merge a ys

msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = let 
                (a,b) = split xs
                d= msort a
                c= msort b
            in
               merge d c

productoEscalar :: (Num a) =>[a]->[a]->a
productoEscalar _ [] = 0
productoEscalar [] _ = 0
productoEscalar (x:xs) (y:ys) = (x*y) + productoEscalar xs ys

prodEscalar :: [Int] -> [Int] -> Int
prodEscalar l1 l2 = sum' [(x*y) | (x,y) <- miZip' l1 l2]

juntar :: (Ord a) => [a]->[a]->[a]
juntar a [] = a
juntar [] b = b
juntar a@(x:xs) b@(y:ys)
                    |x <= y = x: juntar xs b
                    |otherwise = y : juntar a ys

drops::Int ->[a]->[a]
drops _ [] = []
drops 0 l = l
drops n (x:xs) = drops (n-1) xs

delete :: (Eq a) => a -> [a]->[a]
delete v xs= [y|y<-xs, v/= y]