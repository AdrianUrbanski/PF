data  BTree a = Leaf | Node (BTree a) a (BTree a) deriving (Show)

dfnum :: BTree a -> BTree Integer
dfnum t = snd (enum 1 t) where
  enum :: Integer -> BTree a -> (Integer, BTree Integer)
  enum k Leaf = (k-1, Leaf)
  enum k (Node l _ r) =
    let (nlc, nl) = enum (k+1) l
        (nrc, nr) = enum (nlc+1) r
    in
      (nrc, (Node nl k nr))

{-|
-- tworzenie listy asocjacyjnej poddrzewa z liczbą która ma być etykietą jego korzenia w porzadku bfs
createForest :: Integer -> [BTree a] -> [BTree a] -> [(BTree a, Integer)] -> BTree a -> [(BTree a, Integer)]
createForest count [] [] forest Leaf = forest
createForest count [] [] forest (Node l v r) = createForest (count+1) [] [r] (((Node l v r), count):forest) l
createForest count (end:s1) [] forest t = createForest count [] (reverse (end:s1)) forest t
createForest count s1 (front:s2) forest Leaf = createForest count s1 s2 forest front
createForest count s1 (front:s2) forest (Node l v r) = createForest (count+1) (r:l:s1) s2 (((Node l v r), count):forest) front
-}

-- tworzenie listy asocjacyjnej etykiety węzła z liczbą która ma być jego etykietą w porzadku bfs
getLabels :: BTree a -> [(a, Integer)]
getLabels t = assignLabels 1 [] [] [] t where
  assignLabels :: Integer -> [BTree a] -> [BTree a] -> [(a, Integer)] -> BTree a -> [(a, Integer)]
  assignLabels count [] [] labels Leaf = labels
  assignLabels count [] [] labels (Node l v r) = assignLabels (count+1) [] [r] ((v, count):labels) l
  assignLabels count (end:s1) [] labels t = assignLabels count [] (reverse (end:s1)) labels t
  assignLabels count s1 (front:s2) labels Leaf = assignLabels count s1 s2 labels front
  assignLabels count s1 (front:s2) labels (Node l v r) = assignLabels (count+1) (r:l:s1) s2 ((v, count):labels) front

bfnum ::Eq a => BTree a -> BTree Integer
bfnum t = reassemble (getLabels t) t where
  reassemble :: Eq a => [(a, Integer)] -> BTree a -> BTree Integer
  reassemble _ Leaf = Leaf
  reassemble labels (Node l v r) =
    let
      (Just intLabel) = (lookup v labels)
      in
      (Node (reassemble labels l) intLabel (reassemble labels r))

t1 = Node (Node (Node  Leaf 'a' Leaf) 'b' Leaf) 'c' (Node  Leaf 'd' Leaf)
