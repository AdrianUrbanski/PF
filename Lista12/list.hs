import Prelude hiding ((++), head, tail, length, null, (!!))
import qualified Prelude ((++), head, tail, length, null, (!!))

class List l where
  nil :: l a
  cons :: a -> l a -> l a
  head :: l a -> a
  tail :: l a -> l a
  (++) :: l a -> l a -> l a
  (!!) :: l a -> Int -> a
  toList :: [a] -> l a
  fromList :: l a -> [a]

  
instance List [] where
  nil = []
  cons hd tl = hd:tl
  head (hd:_) = hd
  tail (_:tl) = tl
  (++) [] xs = xs
  (++) (x:xs) ys = x:(xs ++ ys)
  (!!) (x:_) 0 = x
  (!!) (_:xs) idx = xs !! (idx-1)
  toList (x:xs) = x:(toList xs)
  fromList (x:xs) = x:(fromList xs)

class List l => SizedList l where
  length :: l a -> Int
  null :: l a -> Bool
  null l = length l == 0

instance SizedList [] where
  length [] = 0
  length (x:xs) = 1+(length xs)
  null [] = True
  null _ = False

data SL l a = SL { len :: Int, list :: l a }

instance List l => List (SL l) where
  nil = SL{len = 0, list = nil}
  cons hd tl = SL{len = (len tl)+1, list = cons hd (list tl)}
  head xs = head (list xs)
  tail xs = SL{len = (len xs)-1, list = tail (list xs)}
  (++) xs ys = SL{len = (len xs)+(len ys), list = (list xs)++(list ys)}
  (!!) xs idx = (!!) (list xs) idx
  toList xs = SL{len = length xs, list = toList xs}
  fromList xs = fromList (list xs)

instance List l => SizedList (SL l) where
  length xs = len xs

infixr 6 :+
data AppList a = Nil | Sngl a | AppList a :+ AppList a

instance Show a => Show (AppList a) where
  show Nil = "[]"
  show xs = "[" ++ showtree xs ++ "]" where
    showtree :: Show a => AppList a -> String
    showtree (Sngl a) = (show a)
    showtree (a1 :+ a2) = showtree a1 ++ ", " ++ showtree a2

instance List AppList where
  nil = Nil
  cons x Nil = Sngl x
  cons x xs = Sngl x :+ xs
  head Nil = error "empty list"
  head (Sngl a) = a
  head (a1 :+ a2) = head a1
  tail Nil = error "empty list"
  tail (Sngl a) = Nil
  tail ((Sngl a) :+ a2) = a2
  tail (a1 :+ a2) = ((tail a1) :+ a2)
  (++) Nil ys = ys
  (++) xs Nil = xs
  (++) xs ys = (xs :+ ys)
  (!!) a idx = fst (find a 0 idx) where
    find :: AppList a -> Int -> Int -> (a, Int)
    find (Sngl a) idx _ = (a, idx)
    find (a1 :+ a2) idx goal = let (lelem, lid) = find a1 idx goal in
      if (lid == goal)
      then (lelem, lid)
      else find a2 (lid+1) goal
  toList [] = nil
  toList (x:xs) = Sngl x :+ toList xs
  fromList Nil = []
  fromList (Sngl a) = [a]
  fromList (a1 :+ a2) = (fromList a1) ++ (fromList a2)

instance SizedList AppList where
  length Nil = 0
  length (Sngl a) = 1
  length (a1 :+ a2) = (length a1) + (length a2)
  null Nil = True
  null _ = False

newtype DiffList a = DL ([a] -> [a])

instance Show a => Show (DiffList a) where
  show (DL f) = show (f [])

instance List DiffList where
  nil = DL id
  cons e (DL f) = DL (\xs -> e:(f xs))
  head (DL f) = head (f [])
  tail (DL f) = DL (tl . f) where
    tl (x:xs) = xs
  (++) (DL f) (DL g) = DL (f . g)
  (!!) (DL f) idx = (f []) !! idx
  toList [] = DL id
  toList (x:xs) = DL (\ys -> x:(unwrap (toList xs) ys)) where
    unwrap (DL f) = f
  fromList (DL f) = f []

instance SizedList DiffList where
  length (DL f) = length (f [])
