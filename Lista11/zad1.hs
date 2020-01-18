ana :: (b -> Maybe (a, b)) -> b -> [a]
ana f st = case f st of
  Nothing  -> []
  Just (v, st') -> v : ana f st'

anazip :: [x] -> [y] -> [(x, y)]
anazip xs ys =
  ana aux (xs, ys) where
  aux :: ([x], [y]) -> Maybe ((x,y), ([x], [y]))
  aux (hdx:tlx, hdy:tly) = Just ((hdx, hdy), (tlx, tly))
  aux ([], []) = Nothing

anaiterate :: (x -> x) -> x -> [x]
anaiterate f e =
  ana (\a -> Just (a, (f a))) e

anamap :: (x -> y) -> [x] -> [y]
anamap f xs =
  ana (aux f) xs where
  aux :: (x -> y) -> [x] -> Maybe (y, [x])
  aux f (hdx:tlx) = Just (f hdx, tlx)
  aux f [] = Nothing

cata :: (a -> b -> b) -> b -> [a] -> b
cata f v [] = v
cata f v (x:xs) = f x (cata f v xs)

catalength :: [x] -> Int
catalength xs =
  cata (\x -> \y -> y+1) 0 xs

catafilter :: (x -> Bool) -> [x] -> [x]
catafilter p xs =
  cata (\x -> \ys -> if p x then x:ys else ys) [] xs where

catamap :: (x -> y) -> [x] -> [y]
catamap f xs =
  cata (\x -> \ys -> (f x):ys) [] xs

data Expr a b = Number b | Var a | Plus (Expr a b) (Expr a b)

data Which a b c = N b | V a | P (c, c)

anaExpr :: Num b => (c -> Which a b c) -> c -> Expr a b
anaExpr f st = case f st of
  N b -> Number b
  V a -> Var a
  P (st1, st2) -> Plus (anaExpr f st1) (anaExpr f st2)

cataExpr :: (a -> c) -> (b -> c) -> (c -> c -> c) -> Expr a b -> c
cataExpr f _ _ (Var a) = f a
cataExpr _ f _ (Number b) = f b
cataExpr f g h (Plus e1 e2) = h (cataExpr f g h e1) (cataExpr f g h e2)

eval :: (Eq a, Num b) => [(a, b)] -> Expr a b -> b
eval env expr =
  cataExpr (\a -> f env a) id (+) expr where
  f :: (Eq x, Num y) => [(x, y)] -> x -> y
  f env a = case (lookup a env) of
    Nothing -> 0
    Just b -> b
