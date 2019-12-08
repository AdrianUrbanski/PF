-- zad 1
f :: [Integer] -> [Integer]
f (hd:tl) =
  [x | x <- tl, x `mod` hd /= 0]

primes :: [Integer]
primes = map head (iterate f [2..])

-- zad 2
primes' :: [Integer]
primes' = 2:[p | p <- [3..], all (\x -> p `mod` x /= 0) (takeWhile (\q -> q*q <= p) primes)]

-- zad 3
fib :: [Integer]
fib = 0:1:zipWith (+) fib (tail fib)

-- zad 4
revApp :: [a] -> [a] -> [a]
revApp [] ys = ys
revApp (x:xs) ys = revApp xs (x:ys)

insert :: [a] -> a -> [[a]]
insert perm elem = aux [] [] perm elem where
  aux :: [[b]] -> [b] -> [b] -> b -> [[b]]
  aux acc beg [] elem = ((revApp beg [elem]):acc)
  aux acc beg (x:end) elem = aux ((revApp beg (elem:x:end)):acc) (x:beg) end elem

iperm :: [a] -> [[a]]
iperm xs = aux [] xs where
  aux :: [[a]] -> [a] -> [[a]]
  aux acc [] = acc
  aux acc (elem:rest) = aux (concat [insert perm elem | perm <- acc]) rest

-- sperm :: [a] -> [[a]]

-- zad 5

-- sublist :: [a] -> [[a]]

-- zad 6

-- qsortBy :: (a -> a -> Bool) -> [a] -> [a]

-- zad 7

{-|
data Tree a = Node (Tree a) a (Tree a) | Leaf

treeEmpty :: Ord a => Tree a
treeEmpty = Leaf

treeAdd :: Ord a => a -> Tree a -> Tree a
treeAdd a (Leaf) = Node Leaf a Leaf
treeAdd a (Node l v r) | a == v = (Node l v r) -- values are unique
                       | a < v = (Node (treeAdd a l) v r)
                       | a > v = (Node l v (treeAdd a r))

treeFromList :: Ord a => [a] -> Tree a
treeFromList xs = foldr treeAdd treeEmpty xs

treeFromSortedList :: Ord a => [a] -> Tree a

listFromTree :: Ord a => Tree a -> [a]
listFromTree Leaf = []
listFromTree (Node l v r) = (listFromTree l)++[v]++(listFromTree r)

treeMerge :: Ord a => Tree a -> Tree a -> Tree a
treeMerge t1 t2 = treeFromList(listUnion [] (listFromTree t1) (listFromTree t2)) where
  listUnion :: Ord a => [] -> [a] -> [a] -> [a]
  listUnion xs [] = xs
  listUnion [] ys =  ys
  listUnion (x:tlxs) (y:tlys) if x<y then x:listUnion tlxs tlys else y:listUnion tlxs tlys

treeIntersection :: Ord a => Tree a -> Tree a -> Tree a
treeIntersection t1 t2


data Set a = Fin (Tree a) | Cofin (Tree a)

setFromList :: Ord a => [a] -> Set a
setFromList xs = Fin(treeFromList xs)

setEmpty :: Ord a => Set a
setEmpty = Fin(treeEmpty)

setFull :: Ord a => Set a
setFull = Cofin(treeEmpty)

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion = Fin(t1) Fin(t2) = Fin(treeMerge t1 t2)
setUnion = Fin(t1) Cofin(t2) = Cofin(treeIntersection t1 t2)
-- s1' U s2 = (s1\s2)'
setUnion = Cofin(t1) Fin(t2) = Cofin(treeIntersection t1 t2)
-- s1 U s2' = (s2\s1)'
setUnion = Cofin(t1) Cofin(t2) = Cofin(treeIntersection t1 t2)
-- s1' U s2' = (s2/\s1)'

setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection Fin(t1) Fin(t2) = Fin(treeIntercetion t1 t2)
setIntersection Cofin(t1) Fin(t2) = Fin(treeIntercetion t1 t2)
-- s1' /\ s2 = s2\s1
setIntersection Fin(t1) Cofin(t2) = Fin(treeIntercetion t1 t2)
-- s1 /\ s2' = s1\s2
setIntersection Cofin(t1) Cofin(t2) = Fin(treeIntercetion t1 t2)
-- s1' /\ s2' = (s1Us2)'
-}
