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

-- l) f :: Num a => a -> a
-- m) \f -> f 7) :: Num t1 => (t1 -> t2) -> t2
-- n) (+ (f 1) (g 1)) :: (Num a, Num t, Num (t -> a)) => a -> a
-- o) (f 7 (g 'x')) :: Num (Char -> t) => t
-- p) (\f -> f (g 1)) :: Num t1 => (t1 -> t2) -> t2
-- q) ((\f -> f (g 1)) square) :: Num t => t