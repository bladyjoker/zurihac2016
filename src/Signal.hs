module Signal where

import Control.Applicative

newtype Signal t a = Sig (t -> a)

instance Functor (Signal t) where
  fmap f (Sig sf) = Sig sf'
    where sf' = (\t -> f (sf t))

instance Applicative (Signal t) where
  pure x = Sig sf
    where sf = (\_ -> x)
  (Sig sfab) <*> (Sig sfa) = Sig sf
    where sf = (\t -> (sfab t) (sfa t))

instance Monad (Signal t) where
  return = pure
  (Sig sf) >>= f = Sig (\t -> let Sig sfb = f (sf t) in sfb t)

-- Evaluation
at' :: t -> Signal t a -> a
at' t (Sig sf) = sf t

-- Construction
constant :: a -> Signal t a
constant x = Sig sf
  where sf = (\_ -> x)

time :: Signal t t
time = Sig sf
  where sf = (\t -> t)

when :: (a -> Bool) -> Signal t a -> Signal t b -> Signal t (Either a b)
when p sa sb = do
  a <- sa
  if p a
    then return (Left a)
    else do {b <- sb; return (Right b)}
         
after :: (Num t, Ord t) => t -> t-> Signal t a -> Signal t b -> Signal t (Either a b)
after tw td sa sb = do
  t <- time
  if t < tw + td
    then return (Left (at' t sa))
    else return (Right (at' t sb))

zipWith :: (a -> b -> c) -> Signal t a -> Signal t b -> Signal t c
zipWith f sa sb = pure f <*> sa <*> sb

-- Sampling
data Event t a = Evt t a
  deriving Show

at :: t -> Signal t a -> Event t a
at t (Sig sf) = Evt t (sf t)

every :: (Num t) => t -> t -> Signal t a -> [Event t a]
every t0 dt (Sig sf) = Evt t0 (sf t0) : (every (t0+dt) dt (Sig sf))

foldh :: (Num t, Ord t) => [Event t a] -> t -> Signal t [Event t a]
foldh es dt = Sig sf
  where sf = (\t -> filter (\(Evt tx _) -> tx >= (t-dt) && tx <= t) es)

