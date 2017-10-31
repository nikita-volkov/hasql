module Hasql.Core.Failing where

import Hasql.Prelude


newtype Failing failure fx success =
  Failing (forall a. (failure -> fx a) -> (fx success -> fx a) -> fx a)

instance Functor fx => Functor (Failing failure fx) where
  {-# INLINE fmap #-}
  fmap fn (Failing fx) =
    Failing (\ fail succeed -> fx fail (succeed . fmap fn))

instance Applicative fx => Applicative (Failing failure fx) where
  {-# INLINE pure #-}
  pure x =
    Failing (\ _ succeed -> succeed (pure x))
  {-# INLINE (<*>) #-}
  (<*>) (Failing leftFx) (Failing rightFx) =
    Failing (\ fail succeed ->
      leftFx fail (\ leftSuccess -> rightFx fail (\ rightSuccess -> succeed (leftSuccess <*> rightSuccess))))

instance Monad fx => Monad (Failing failure fx) where
  {-# INLINE return #-}
  return x =
    Failing (\ _ succeed -> succeed (return x))
  {-# INLINE (>>=) #-}
  (>>=) (Failing leftFx) rightK =
    Failing $ \ fail succeed ->
    leftFx fail $ \ leftSuccessFx ->
    do
      leftSuccess <- leftSuccessFx
      case rightK leftSuccess of
        Failing rightFx -> rightFx fail succeed

instance MonadTrans (Failing failure) where
  {-# INLINE lift #-}
  lift fx =
    Failing (\ _ succeed -> succeed fx)
