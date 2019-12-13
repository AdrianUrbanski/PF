import Data.List (inits, tails)

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
insert :: [a] -> a -> [[a]]
insert xs elem = zipWith (\init -> \tail -> init++[elem]++tail) (inits xs) (tails xs)

iperm :: [a] -> [[a]]
iperm xs = aux [[]] xs where
  aux :: [[a]] -> [a] -> [[a]]
  aux acc [] = acc
  aux acc (elem:rest) = aux (concat [insert perm elem | perm <- acc]) rest

iperm' :: [a] -> [[a]]
iperm' xs = aux [[]] xs where
  aux :: [[a]] -> [a] -> [[a]]
  aux acc [] = acc
  aux acc (elem:rest) = aux (concatMap (\xs -> insert xs elem) acc) rest

select :: ([a],[a]) -> [([a],[a])]
select (xs, ys) = aux xs [] [] ys where
  aux :: [a] -> [([a], [a])] -> [a] -> [a] -> [([a], [a])]
  aux xs acc init [] = acc
  aux xs acc init (elem:tail) = aux xs ((elem:xs, (reverse init)++tail):acc) (elem:init) tail

sperm :: [a] -> [[a]]
sperm xs = map fst (aux [([], xs)]) where
  aux :: [([a],[a])] -> [([a],[a])]
  aux xs = if (snd (head xs)) == []
           then xs
           else concatMap select xs

-- zad 5

sublists :: [a] -> [[a]]
sublists xs = map reverse (aux [[]] xs) where
  aux :: [[a]] -> [a] -> [[a]]
  aux acc [] = acc
  aux acc (elem:rest) = aux (acc++[elem:x | x <- acc]) rest

-- zad 6

qsortBy :: (a -> a -> Bool) -> [a] -> [a]
qsortBy f [] = []
qsortBy f (x:[]) = (x:[])
qsortBy f (hd:tl) = (qsortBy f [x | x <- tl, f hd x])++[hd]++(qsortBy f [x | x <- tl, not(f hd x)])

-- zad 7

{-|
data Tree a = Node (Tree a) a (Tree a) | Leaf

treeEmpty :: Ord a => Tree a
treeEmpty = Leaf

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert a (Leaf) = Node Leaf a Leaf
treeInsert a (Node l v r) | a == v = (Node l v r) -- values are unique
                       | a < v = (Node (treeAdd a l) v r)
                       | a > v = (Node l v (treeAdd a r))

treeRemove :: Ord a => a -> Tree a -> Tree a

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
setUnion = Fin(t1) Cofin(t2) = Cofin(treeSub t1 t2)
-- s1' U s2 = (s1\s2)'
setUnion = Cofin(t1) Fin(t2) = Cofin(treesub t2 t1)
-- s1 U s2' = (s2\s1)'
setUnion = Cofin(t1) Cofin(t2) = Cofin(treeIntersection t1 t2)
-- s1' U s2' = (s2/\s1)'

setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection Fin(t1) Fin(t2) = Fin(treeIntercetion t1 t2)
setIntersection Cofin(t1) Fin(t2) = Fin(treeSub t1 t2)
-- s1' /\ s2 = s2\s1
setIntersection Fin(t1) Cofin(t2) = Fin(treeSub t1 t2)
-- s1 /\ s2' = s1\s2
setIntersection Cofin(t1) Cofin(t2) = Cofin(treeSum t1 t2)
-- s1' /\ s2' = (s1Us2)'
-}
