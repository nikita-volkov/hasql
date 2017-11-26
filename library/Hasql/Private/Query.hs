module Hasql.Private.Query
where

import Hasql.Private.Prelude
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Hasql.Private.IO as IO
import qualified Hasql.Private.Connection as Connection
import qualified Hasql.Private.Decoders.Results as Decoders.Results
import qualified Hasql.Private.Encoders.Params as Encoders.Params


-- |
-- An abstraction over parametric queries.
-- 
-- It is composable using
-- the standard interfaces of the category theory,
-- which it has instances of.
-- E.g., here's how you can compose queries
-- using the Arrow notation:
-- 
-- @
-- -- |
-- -- Given an Update query,
-- -- which uses the \@fmap (> 0) 'Decoders.Results.rowsAffected'\@ decoder
-- -- to detect, whether it had any effect,
-- -- and an Insert query,
-- -- produces a query which performs Upsert.
-- composeUpsert :: Query a Bool -> Query a () -> Query a ()
-- composeUpsert update insert =
--   proc params -> do
--     updated <- update -< params
--     if updated
--       then 'returnA' -< ()
--       else insert -< params
-- @
newtype Query a b =
  Query (Kleisli (ReaderT Connection.Connection (ExceptT Decoders.Results.Error IO)) a b)
  deriving (Category, Arrow, ArrowChoice, ArrowLoop, ArrowApply)

instance Functor (Query a) where
  {-# INLINE fmap #-}
  fmap =
    (^<<)

instance Profunctor Query where
  {-# INLINE lmap #-}
  lmap =
    (^>>)
  {-# INLINE rmap #-}
  rmap =
    (^<<)

statement :: ByteString -> Encoders.Params.Params a -> Decoders.Results.Results b -> Bool -> Query a b
statement template encoder decoder preparable =
  Query $ Kleisli $ \params -> 
    ReaderT $ \(Connection.Connection pqConnectionRef integerDatetimes registry) -> 
      ExceptT $ withMVar pqConnectionRef $ \pqConnection -> do
        r1 <- IO.sendParametricQuery pqConnection integerDatetimes registry template encoder preparable params
        r2 <- IO.getResults pqConnection integerDatetimes decoder
        return $ r1 *> r2

