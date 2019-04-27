-- ZAD 7
fib1 0 = 0
fib1 1 = 1
fib1 x = fib1 (x - 1) + fib1 (x - 2)
fib_r x = fib1 x 


fib2 0 y z = z
fib2 x y z = fib2 (x - 1) z (y + z)
fib_t x = fib2 x 1 0


-- ZAD 8
-- a) append l m = l ++ m
append1 [] m = m
append1 (l:ls) m = l : (append1 ls m) 

-- b) member
member1 x [] = False
member1 x (l:ls) = 
    if (x == l) then True
    else member1 x ls

-- c) reverse
reverse1 [] = []
reverse1 (x:xs) = (reverse1 xs) ++ [x]

-- d) last
last1 [x] = x
last1 (x:xs) = last1 xs

-- e) delete
delete1 x left [] = left
delete1 x left (r:right) =
    if (x == r)
        then left ++ right
        else delete1 x (left ++ [r]) right
     
delete2 x ls = delete1 x [] ls

-- f) split
split1 _ [] = ([],[])
split1 pivot ls = ([left | left <- ls, left < pivot], [right | right <- ls, right >= pivot])

-- g) map
map1 _ [] = []
map1 f (x:xs) = (f x) : (map1 f xs)


-- ZAD 9


-- ZAD 10


-- ZAD 11


-- ZAD 12
foldr2 f acc [] = acc
foldr2 f acc (x:xs) = f x (foldr2 f acc xs)


-- ZAD 13
-- a)
prodl [] = 0
prodl (x:xs) = foldl (*) 1 (x:xs)

prodr [] = 0
prodr (x:xs) = foldr (*) 1 (x:xs)

-- b)
append2 x y = y : x
reversel [] = []
reversel xs = foldl append2 [] xs

-- c)
and1 a b = a == True && b == True

andl [] = False
andl xs = foldl (and1) True xs

andr [] = False
andr xs = foldr (and1) True xs

-- d)

-- e)

-- f)

-- g)

-- h)
filter1 pred [] = []
filter1 pred (x:xs) = foldl (\x xs -> if pred x then x else xs) [] xs

-- i)



-- ZAD 14


-- ZAD 15


-- ZAD 16


-- ZAD 17
square x = x * x
f x = x
g x = x

-- a) (+) :: Num a => a -> a -> a
-- b) (+ 37) :: Num a => a -> a
-- c) append1 :: [a] -> [a] -> [a]
-- d) append1 [1,2] :: Num a => [a] -> [a]
-- e) map :: (a -> b) -> [a] -> [b]
-- f) (map square [1,2,3,4,5]) :: Num b => [b]
-- g) ?
-- h) map length [['a']] :: [Int]
-- i) foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
-- j) foldl (++) :: Foldable t => [a] -> t [a] -> [a]
-- k) foldl (++) [] :: Foldable t => t [a] -> [a]

-- l) e = f 7
-- l) { f :: (Int -> a)} |- e :: a

-- m) e = (\f -> f 7)
-- m) { } |- e :: (Int -> a) -> a

-- n) e = + (f x) (g x)
-- n) { f :: (a -> Int), g :: (a -> Int), x :: a } |- e :: Int

-- o) e = f 7 (g 'x')
-- o) { f :: Int -> b -> c, g :: Char -> b} |- e :: c

-- p) e = \f -> f (g x)
-- p) { g :: (c -> a), x :: c } |- e :: (a -> b) -> b

-- q) e = (\f -> f (g x)) square
-- q) { g :: (c -> a), x :: c} |- e :: Int

-- n) + (f x) (g x) 
-- n) (a -> b) -> (a -> b) -> b


-- ZAD 18
--
-- ALGORYTM
-- 1. Sprawdzamy czy funkcja zewnetrzna jest taka sama. Jesli nie - nie da sie.
-- 2. Jesli jest f(x, g(a)) i f(y, g(b)) to argumenty g (tj a i b) musza sie unifikowac.
-- 
-- a)
-- t1 = f(x, g(b))
-- t2 = f(a, y)
--
-- x =fi= a
-- fi(x) = a
--
-- g(b) =fi= y
-- fi(y) = g(b)
--
-- => fi(t1) === fi(t2)

-- b)
-- f( h(b, x), y) i f( h(a,y), x)
-- h(b, x) =fi= h(a, y)
-- fi(b) = a
-- fi(x) = y

-- Zad 22
data BinTree a = Leaf a | Node a (BinTree a) (BinTree a)

-- a)
heightBinTree (Leaf x) = 1
heightBinTree (Node x l r) = 1 + max (heightBinTree l) (heightBinTree r)

-- b)
sizeBinTree (Leaf x) = 1
sizeBinTree (Node x l r) = 1 + (sizeBinTree l) + (sizeBinTree r)

-- c)
maxBinTree (Leaf x) = x
maxBinTree (Node x l r) = max x (max (maxBinTree l) (maxBinTree r))

-- d)
preBinTree (Leaf x) = [x]
preBinTree (Node x l r) = (x : preBinTree l) ++ preBinTree r

-- e)
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Node x l r) = Node (f x) (mapTree f l) (mapTree f r)

-- f)
foldTree f g (Leaf x) = f x
foldTree f g (Node x l r) = (g x (foldTree f g l) (foldTree f g r))
-- let y = Node 1 (Node 2 (Leaf 10) (Leaf 11)) (Node 3 (Leaf 4) (Node 5 (Leaf 6) (Leaf 7)))
-- foldTree (\x -> x) (\x y z -> x + y + z) (Leaf 5)
-- foldTree (\x -> x) (\x y z -> y + z) (Leaf 5)
-- oldTree (\x -> x) (\x y z -> x + y + z) y

-- Zad 23
data Set a = S [a]
member x (S l) = elem x l