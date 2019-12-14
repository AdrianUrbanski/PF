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

{-|
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
-}

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

data Tree a = Node (Tree a) a (Tree a) | Leaf deriving (Show)

treeEmpty :: Ord a => Tree a
treeEmpty = Leaf

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert a Leaf = Node Leaf a Leaf
treeInsert a (Node l v r) | a == v = (Node l v r) -- values are unique
                       | a < v = (Node (treeInsert a l) v r)
                       | a > v = (Node l v (treeInsert a r))

treeFromList :: Ord a => [a] -> Tree a
treeFromList xs = foldr treeInsert treeEmpty xs

treeJoin :: Ord a => Tree a -> Tree a -> Tree a
treeJoin l Leaf = l
treeJoin Leaf r = r
treeJoin l (Node rl rv rr) = (Node (treeJoin l rl) rv rr)

treeJoinM :: Ord a => Tree a -> a -> Tree a -> Tree a
treeJoinM l v r = treeJoin l (treeJoin (Node Leaf v Leaf) r)

treeSplit :: Ord a => Tree a -> a -> (Tree a, Bool, Tree a)
treeSplit Leaf k = (Leaf, False, Leaf)
treeSplit (Node l v r) k | v == k = (l, True, r)
                         | v < k =
                             let (lt, b, rt) = treeSplit r k in
                               ((treeJoin (Node l v Leaf) lt), b, rt)
                         | v > k =
                             let (lt, b, rt) = treeSplit l k in
                               (lt, b, (treeJoin rt (Node Leaf v r)))

treeUnion :: Ord a => Tree a -> Tree a -> Tree a
treeUnion l Leaf = l
treeUnion Leaf r = r
treeUnion (Node ll lv lr) r =
  let (rl, _, rr) = treeSplit r lv
      (nl, nr) = ((treeUnion ll rl), (treeUnion lr rr))
  in treeJoinM nl lv nr

treeIntersection :: Ord a => Tree a -> Tree a -> Tree a
treeIntersection Leaf _ = Leaf
treeIntersection _ Leaf = Leaf
treeIntersection (Node ll lv lr) r =
  let (rl, b, rr) = treeSplit r lv
      (nl, nr) = ((treeIntersection ll rl), (treeIntersection lr rr))
  in if b then treeJoinM nl lv nr else treeJoin nl nr

treeDifference :: Ord a => Tree a -> Tree a -> Tree a
treeDifference Leaf _ = Leaf
treeDifference l Leaf = l
treeDifference (Node ll lv lr) r =
  let (rl, b, rr) = treeSplit r lv
      (nl, nr) = ((treeDifference ll rl), (treeDifference lr rr))
  in if b then treeJoin nl nr else treeJoinM nl lv nr

treeFind :: Ord a => a -> Tree a -> Bool
treeFind a Leaf = False
treeFind a (Node l v r) | a == v = True
                        | a < v = (treeFind a l)
                        | a > v = (treeFind a r)

t1 = Node (Node (Node Leaf 1 Leaf) 5 (Node Leaf 7 Leaf))  8 (Node Leaf 9 Leaf)
t2 = Node (Node Leaf 5 Leaf) 6 (Node Leaf 8 Leaf)

data Set a = Fin (Tree a) | Cofin (Tree a) deriving (Show)

setFromList :: Ord a => [a] -> Set a
setFromList xs = Fin (treeFromList xs)

setEmpty :: Ord a => Set a
setEmpty = Fin treeEmpty

setFull :: Ord a => Set a
setFull = Cofin treeEmpty

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Fin t1) (Fin t2) = Fin (treeUnion t1 t2)
setUnion (Fin t1) (Cofin t2) = Cofin (treeDifference t2 t1) -- s1 U s2' = (s2\s1)'
setUnion (Cofin t1) (Fin t2) = Cofin (treeDifference t1 t2) -- s1' U s2 = (s1\s2)'
setUnion (Cofin t1) (Cofin t2) = Cofin (treeIntersection t1 t2) -- s1' U s2' = (s2/\s1)'

setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Fin t1) (Fin t2) = Fin (treeIntersection t1 t2)
setIntersection (Cofin t1) (Fin t2) = Fin (treeDifference t2 t1) -- s1' /\ s2 = s2\s1
setIntersection (Fin t1) (Cofin t2) = Fin (treeDifference t1 t2) -- s1 /\ s2' = s1\s2
setIntersection (Cofin t1) (Cofin t2) = Cofin (treeUnion t1 t2) -- s1' /\ s2' = (s1Us2)'

setComplement :: Ord a => Set a -> Set a
setComplement (Fin t) = Cofin t
setComplement (Cofin t) = Fin t

setMember :: Ord a => a -> Set a -> Bool
setMember k (Fin t) = treeFind k t
setMember k (Cofin t) = not (treeFind k t)

s1 = setFromList [1, 5, 7, 8, 9]
s2 = setFromList [5, 6, 8]
