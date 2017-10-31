module Hasql.Core.Fail where

import Hasql.Prelude


newtype Fail failure success =
  Fail (forall a. (failure -> a) -> (success -> a) -> a)

instance Functor (Fail failure) where
  {-# INLINE fmap #-}
  fmap fn (Fail result) =
    Fail (\ failureResult successResult -> result failureResult (successResult . fn))

instance Applicative (Fail failure) where
  {-# INLINE pure #-}
  pure x =
    Fail (\ _ successResult -> successResult x)
  {-# INLINE (<*>) #-}
  (<*>) (Fail leftResult) (Fail rightResult) =
    Fail (\ failureResult successResult ->
      leftResult failureResult (\ leftSuccess -> rightResult failureResult (\ rightSuccess -> successResult (leftSuccess rightSuccess))))

instance Monad (Fail failure) where
  {-# INLINE return #-}
  return = pure
  {-# INLINE (>>=) #-}
  (>>=) (Fail leftResult) rightK =
    Fail $ \ failureResult successResult ->
    leftResult failureResult $ \ leftSuccess ->
    do
      case rightK leftSuccess of
        Fail rightResult -> rightResult failureResult successResult
