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

constant :: a -> Signal t a
constant x = Sig sf
  where sf = (\_ -> x)

time :: Signal t t
time = Sig sf
  where sf = (\t -> t)

data Event t a = Evt t a
  deriving Show

at :: t -> Signal t a -> Event t a
at t (Sig sf) = Evt t (sf t)

every :: (Num t) => t -> t -> Signal t a -> [Event t a]
every t0 dt (Sig sf) = Evt t0 (sf t0) : (every (t0+dt) dt (Sig sf))

foldh :: (Num t, Ord t) => [Event t a] -> t -> Signal t [Event t a]
foldh es dt = Sig sf
  where sf = (\t -> filter (\(Evt tx _) -> tx >= (t-dt) && tx <= t) es)
