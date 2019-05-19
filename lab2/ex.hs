-- ##
-- 8
-- ##

append' :: [a] -> [a] -> [a]
append' [] m = m
append' (l:ls) m = l : (append' ls m)

member' :: Eq a => a -> [a] -> Bool
member' _ [] = False
member' x (l:ls) = if x == l then True else member' x ls

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

last' :: [a] -> a
last' [] = error "empty list"
last' [x] = x
last' (_:xs) = last' xs

delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' x (y:ys) = if x == y then ys else y : (delete' x ys)

splitt' :: Ord a => a -> [a] -> [a] -> [a] -> [[a]]
splitt' _ [] left right = [left, right]
splitt' x (y:ys) left right = if y > x then splitt' x ys left (y : right) else splitt' x ys (y : left) right
-- this is just a wrapper that accepts two params (pivot and the list)
split' :: Ord a => a -> [a] -> [[a]]
split' x l = splitt' x l [] []

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = (f x) : (map' f xs)


-- ##
-- 9
-- ##

map2' :: (a -> b -> c) -> [a] -> [b] -> [c]
map2' _ [] [] = []
map2' _ [] (_:_) = error "2nd list is longer than 1st list!"
map2' _ (_:_) [] = error "1nst list is longer than 2nd list!"
map2' f (x:xs) (y:ys) = (f x y) : (map2' f xs ys)

pairing' :: [a] -> [b] -> [(a,b)]
pairing' [] [] = []
pairing' [] (_:_) = error "2nd list is longer than 1st list!"
pairing' (_:_) [] = error "1st list is longer than 2nd list!"
pairing' (x:xs) (y:ys) = (x,y) : (pairing' xs ys)


-- ##
-- 11
-- ##

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) = if (p x) then x : (filter' p xs) else (filter' p xs)


-- ##
-- 12
-- ##

-- [1,2,3] => (1 + (2 + (3 + 0))
-- foldr' (+) 0 [1,2,3] => (+) 1 (foldr' (+) 0 [2,3])
-- (+) 1 (foldr' (+) 0 [2,3]) => (+) 1 ((+) 2 (foldr' (+) 0 [3]))
-- (+) 1 ((+) 2 (foldr' (+) 0 [3])) => (+) 1 ((+) 2 ((+) 3 (foldr' (+) 0 [])))
-- (+) 1 ((+) 2 ((+) 3 (foldr' (+) 0 []))) => (+) 1 ((+) 2 ((+) 3 0))
foldr' :: (a -> a -> a) -> a -> [a] -> a
foldr' _ acc [] = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)

-- foldl' (+) 0 [1,2,3] => foldl' (+) ((+) 0 1) [2,3]
-- foldl' (+) 1 [2,3] => foldl' (+) ((+) 1 2) [3]
-- foldl' (+) 3 [3] => foldl' (+) ((+) 3 3) []
-- foldl' (+) 6 [] => 6
foldl' :: (a -> a -> a) -> a -> [a] -> a
foldl' _ acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs


-- ##
-- 13
-- ##
prodl' :: Num a => [a] -> a
prodl' [] = 0
prodl' (x:xs) = foldl' (*) 1 (x:xs)

prodr' :: Num a => [a] -> a
prodr' [] = 0
prodr' (x:xs) = foldr' (*) 1 (x:xs)
