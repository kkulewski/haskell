---
---
member' x [] = False
member' x (y:ys) = if (x == y) then True else member' x ys

delete' x [] = []
delete' x (y:ys) = if (x == y) then delete' x ys else y : (delete' x ys)

exists' pred [] = False
exists' pred (y:ys) = if (pred y) then True else exists' pred ys

-- like that
cut' [a] = []
cut' (y:ys) = y : (cut' ys)
cutt' [] = []
cutt' [a] = []
cutt' (y:ys) = cut' ys

-- or that
cuttt (y:ys) = reverse ys
cutt [] = []
cutt [a] = []
cutt (y:ys) = cuttt (reverse ys)



-- if 'init' function is allowed, this is trivial. if not, here is the implementation:
-- ABCBA -> x=A, XS=BCBA
-- reverse BCBA (->ABCB), remove first element with lambda pattern match (BCB), reverse again to make sure its not modified (BCB)
-- reverse ((lambda) (reversed xs))
palindrom' [] = True 
palindrom' [a] = True
palindrom' (x:xs) = if (x == last xs) then palindrom' (reverse ((\(y:ys) -> ys) (reverse xs))) else False 


--
data Tree a b = Leaf a | Node a b (Tree a b) (Tree a b)

sumTree (Leaf a) = a
sumTree (Node a b left right) = a + sumTree left + sumTree right

mt = Leaf 5
mr = Node 5 3 (Node 50 51 (Leaf 52) (Leaf 53)) (Node 2 4 (Leaf 1) (Leaf 2))

preTree (Leaf a) = [a]
preTree (Node a b left right) = a : b : (preTree left) ++ (preTree right)

mapb f (Leaf a) = Leaf a
mapb f (Node a b left right) = Node a (f b) (mapb f left) (mapb f right)
-- to print, pass mapb (with lambda) result to preTree e.g.
-- preTree (mapb (\x -> x + 1) mr)

--
-- (\f -> f x)
-- schema: e, f e | f
-- e is x :: a
-- f is a -> b
