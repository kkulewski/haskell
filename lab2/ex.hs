{-# LANGUAGE RankNTypes #-}

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


-- ##
-- 21
-- ##

summ' :: Num c => (forall a. [a] -> c) -> [x] -> [y] -> c
summ' f l1 l2 = (f l1) + (f l2)


-- ##
-- 22
-- ##

data BinTree a = Leaf a | Node (BinTree a) (BinTree a)

heightBinTree :: Num a => Num p => Ord p => BinTree a -> p
heightBinTree (Leaf _) = 1
heightBinTree (Node left right) = 1 + max (heightBinTree left) (heightBinTree right)

sizeBinTree :: BinTree a -> Int
sizeBinTree (Leaf _) = 1
sizeBinTree (Node left right) = 1 + (sizeBinTree left) + (sizeBinTree right)

sumBinTree :: Num a => BinTree a -> a
sumBinTree (Leaf a) = a
sumBinTree (Node left right) = (sumBinTree left) + (sumBinTree right)

preBinTree :: BinTree a -> [a]
preBinTree (Leaf a) = [a]
preBinTree (Node left right) = preBinTree left ++ preBinTree right

mapBinTree :: (a -> b) -> BinTree a -> BinTree b
mapBinTree f (Leaf a) = Leaf (f a)
mapBinTree f (Node left right) = Node (mapBinTree f left) (mapBinTree f right)


-- ##
-- 23
-- ##

data Tree a = TreeLeaf a | TreeNode a (Tree a)

sizeTree :: Tree a -> Int
sizeTree (TreeLeaf _) = 1
sizeTree (TreeNode _ son) = 1 + sizeTree son

sumTree :: Num a => Tree a -> a
sumTree (TreeLeaf a) = a
sumTree (TreeNode a son) = a + sumTree son

preTree :: Tree a -> [a]
preTree (TreeLeaf a) = [a]
preTree (TreeNode a son) = a : (preTree son)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (TreeLeaf a) = TreeLeaf (f a)
mapTree f (TreeNode a son) = TreeNode (f a) (mapTree f son)


-- ##
-- 24
-- ##

data Set a = S [a]

setToList :: Set a -> [a]
setToList (S []) = []
setToList (S (x:xs)) = x : setToList (S xs)

setMember :: Eq a => a -> Set a -> Bool
setMember _ (S []) = False
setMember x (S (y:ys)) = if x == y then True else setMember x (S ys)

setSubset :: Eq a => Set a -> Set a -> Bool
setSubset (S []) (S _) = True
setSubset (S (x:xs)) (S ys) = if setMember x (S ys) then setSubset (S xs) (S ys) else False

setUnion :: Eq a => Set a -> Set a -> Set a
setUnion (S []) (S ys) = S ys
setUnion (S (x:xs)) (S ys) = if setMember x (S ys) then setUnion (S xs) (S ys) else setUnion (S xs) (S (ys ++ [x]))

setIntersection' :: Eq a => Set a -> Set a -> Set a -> Set a
setIntersection' (S []) (S _) (S zs) = S zs
setIntersection' (S (x:xs)) (S ys) (S zs) = if setMember x (S ys)
    then setIntersection' (S xs) (S ys) (S (zs ++ [x]))
    else setIntersection' (S xs) (S ys) (S zs)

setIntersection :: Eq a => Set a -> Set a -> Set a
setIntersection (S xs) (S ys) = setIntersection' (S xs) (S ys) (S [])


setAppend :: a -> Set a -> Set a
setAppend x (S []) = S [x]
setAppend x (S ys) = S (x:ys)

setDelete :: Eq a => a -> Set a -> Set a
setDelete _ (S []) = S []
setDelete x (S (y:ys)) = if y == x then setDelete x (S ys) else setAppend y (setDelete x (S (ys)))
