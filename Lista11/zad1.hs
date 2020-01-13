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

data  Expr a b = Number b | Var a | Plus (Expr a b) (Expr a b)

anaExpr :: Num b => (Expr a b -> Maybe(b, Expr a b)) -> Expr a b -> b
anaExpr f st = case f st of
  Nothing -> 0
  Just (v, st') -> 

