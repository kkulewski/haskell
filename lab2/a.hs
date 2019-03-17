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
