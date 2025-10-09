module PercentEncoding.MonadPlus where

import Platform.Prelude hiding (scanl)

{-# INLINE scanl #-}
scanl :: (MonadPlus m) => (a -> b -> a) -> a -> m b -> m a
scanl step start subaction =
  loop start
  where
    loop state =
      mplus
        ( do
            element <- subaction
            loop $! step state element
        )
        (return state)

{-# INLINE scanl1 #-}
scanl1 :: (MonadPlus m) => (a -> a -> a) -> m a -> m a
scanl1 step subaction = do
  start <- subaction
  scanl step start subaction

{-# INLINE scanlM #-}
scanlM :: (MonadPlus m) => (a -> b -> m a) -> a -> m b -> m a
scanlM step start subaction =
  loop start
  where
    loop state =
      join
        ( mplus
            ( do
                element <- subaction
                return (step state element >>= loop)
            )
            (return (return state))
        )

{-# INLINE scan #-}
scan :: (MonadPlus m, Monoid a) => m a -> m a
scan = scanl mappend mempty
