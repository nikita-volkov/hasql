module Hasql.Choosing where

import Hasql.Prelude


newtype Choosing input m output = Choosing (input -> Maybe (m output))

instance (Functor m) => Functor (Choosing input m) where
  {-# INLINE fmap #-}
  fmap mapping (Choosing choice) =
    Choosing (fmap (fmap mapping) . choice)

instance (Applicative m) => Applicative (Choosing input m) where
  {-# INLINE pure #-}
  pure x =
    Choosing (const (Just (pure x)))
  {-# INLINE (<*>) #-}
  (<*>) (Choosing left) (Choosing right) =
    Choosing $ \input ->
    case left input of
      Just leftM -> case right input of
        Just rightM -> Just (leftM <*> rightM)
        Nothing -> Nothing
      Nothing -> Nothing

instance (Applicative m) => Alternative (Choosing input m) where
  {-# INLINE empty #-}
  empty =
    Choosing (const Nothing)
  {-# INLINE (<|>) #-}
  (<|>) (Choosing left) (Choosing right) =
    Choosing $ \input ->
    case left input of
      Just leftM -> Just leftM
      Nothing -> case right input of
        Just rightM -> Just rightM
        Nothing -> Nothing
