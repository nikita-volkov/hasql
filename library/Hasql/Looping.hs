module Hasql.Looping where

import Hasql.Prelude

newtype Looping m result =
  Looping (m (Either result (Looping m result)))

instance (Functor m) => Functor (Looping m) where
  {-# INLINE fmap #-}
  fmap mapping (Looping m) =
    Looping (fmap (either (Left . mapping) (Right . fmap mapping)) m)

instance (Applicative m) => Applicative (Looping m) where
  {-# INLINE pure #-}
  pure x =
    Looping (pure (Left x))
  {-# INLINE (<*>) #-}
  (<*>) (Looping left) rightLooping =
    Looping $ flip fmap left $ \case
      Left leftOutput -> Right (fmap leftOutput rightLooping)
      Right leftLooping -> Right (leftLooping <*> rightLooping)

instance (Alternative m) => Alternative (Looping m) where
  {-# INLINE empty #-}
  empty =
    Looping empty
  {-# INLINE (<|>) #-}
  (<|>) (Looping left) (Looping right) =
    Looping (left' <|> right')
    where
      left' =
        fmap (fmap (<|> Looping right)) left
      right' =
        fmap (fmap (Looping left <|>)) right

instance (Applicative m) => Monad (Looping m) where
  {-# INLINE return #-}
  return = pure
  {-# INLINE (>>=) #-}
  (>>=) (Looping left) rightK =
    Looping $ flip fmap left $ \case
      Left leftOutput -> Right (rightK leftOutput)
      Right nextLeftLooping -> Right (nextLeftLooping >>= rightK)

instance (Alternative m) => MonadPlus (Looping m) where
  mzero = empty
  mplus = (<|>)
