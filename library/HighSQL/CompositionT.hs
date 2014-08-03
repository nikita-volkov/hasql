module HighSQL.CompositionT where

import HighSQL.Prelude


-- |
-- A monad transformer, 
-- which serves a purpose of detecting composition of the inner monad.
data T m r =
  T Bool (m r)

-- |
-- Unwrap into a boolean signifying whether the base monad is a composition and
-- the base monad itself.
run :: T m r -> (Bool, m r)
run (T c m) = (c, m)

instance Monad m => Monad (T m) where
  return a =
    T False (return a)
  (>>=) (T _ m) k =
    T True (m >>= \a -> case k a of T _ m' -> m')

instance Functor f => Functor (T f) where
  fmap f (T c m) = T c (fmap f m)

instance Applicative f => Applicative (T f) where
  pure a =
    T False (pure a) 
  (<*>) (T _ a) (T _ b) = 
    T True (a <*> b)

instance MonadTrans T where
  lift m =
    T False m

instance MonadIO m => MonadIO (T m) where
  liftIO io =
    T False (liftIO io)

