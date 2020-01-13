data Array a = Leaf | Node (Array a) a (Array a) | Root (Array a) (a, Integer) (Array a) deriving (Show)

aempty :: Array a
aempty = (Root Leaf (undefined, 1) Leaf)

asub :: Array a -> Integer -> a
asub Leaf _ = undefined
asub (Root _ (elem, _) _) 1 = elem
asub (Root l _ r) idx =
  if (mod idx 2) == 1
  then asub r (div idx 2)
  else asub l (div idx 2)
asub (Node _ elem _) 1 = elem
asub (Node l _ r) idx =
  if (mod idx 2) == 1
  then asub r (div idx 2)
  else asub l (div idx 2)

aupdate :: Array a -> Integer -> a -> Array a
aupdate Leaf _ _ = undefined
aupdate (Root l (_, size) r) 1 val = (Root l (val, size) r)
aupdate (Root l m r) idx val =
  if (mod idx 2) == 1
  then (Root l m (aupdate r (div idx 2) val))
  else (Root (aupdate l (div idx 2) val) m r)
aupdate (Node l _ r) 1 val = (Node l val r)
aupdate (Node l m r) idx val =
  if (mod idx 2) == 1
  then (Node l m (aupdate r (div idx 2) val))
  else (Node (aupdate l (div idx 2) val) m r)

ahiext :: Array a -> a -> Array a
ahiext (Root _ (_, 1) _) val = (Root Leaf (val, 2) Leaf)
ahiext (Root l (elem, idx) r) val =
  if (mod idx 2) == 1
  then (Root l (elem, (idx+1)) (aux r (div idx 2) val))
  else (Root (aux l (div idx 2) val) (elem, (idx+1)) r)
  where
    aux :: Array a -> Integer -> a -> Array a
    aux Leaf 1 val = (Node Leaf val Leaf)
    aux (Node l m r) idx val =
      if (mod idx 2) == 1
      then (Node l m (aux r (div idx 2) val))
      else (Node (aux l (div idx 2) val) m r)

ahirem :: Array a -> Array a
ahirem (Root _ (_, 2) _) = (Root Leaf (undefined, 1) Leaf)
ahirem (Root l (elem, idx) r) =
  if (mod (idx-1) 2) == 1
  then (Root l (elem, (idx-1)) (aux r (div (idx-1) 2)))
  else (Root (aux l (div (idx-1) 2)) (elem, (idx-1)) r)
  where
    aux :: Array a -> Integer -> Array a
    aux a 1 = Leaf
    aux (Node l m r) idx =
      if (mod idx 2) == 1
      then (Node l m (aux r (div idx 2)))
      else (Node (aux l (div idx 2)) m r)

a1 = ahiext (ahiext (ahiext aempty 1) 2) 3
