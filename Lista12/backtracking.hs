class (Monad m) => MonadPlus m where
   mzero :: m a
   mplus :: m a -> m a -> m a

-- iperm, sperm :: MonadPlus m => [a] -> m [a]

type  Symbol = String
data  Term v = Var v | Sym  Symbol [Term v]

instance Functor Term where
  fmap f (Var v) = Var (f v)
  fmap f (Sym s terms) = Sym s (map (\t -> fmap f t) terms)

instance Applicative Term where
  pure v = Var v
  Var f <*> Var v = Var (f v)
  vf <*> (Sym s terms) = Sym s (map (vf <*>) terms)
  
instance Monad Term where
  Var v >>= f = f v
  Sym s terms >>= f = Sym s (map (>>= f) terms)
  return v = Var v
