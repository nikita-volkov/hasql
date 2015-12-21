module Hasql.Private.Session
where

import Hasql.Prelude
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Hasql.Decoders.Results as Decoders.Results
import qualified Hasql.Settings as Settings
import qualified Hasql.Private.IO as IO
import qualified Hasql.Private.Query as Query
import qualified Hasql.Private.PreparedStatementRegistry as PreparedStatementRegistry


-- |
-- A batch of actions to be executed in the context of a database connection.
newtype Session a =
  Session
    (ReaderT
      (LibPQ.Connection, Bool, PreparedStatementRegistry.PreparedStatementRegistry)
      (EitherT Decoders.Results.Error IO)
      a)
  deriving (Functor, Applicative, Monad)

-- |
-- Possibly a multi-statement query,
-- which however cannot be parameterized or prepared,
-- nor can any results of it be collected.
sql :: ByteString -> Session ()
sql sql =
  Session $ ReaderT $ \(pqConnection, integerDatetimes, registry) ->
    EitherT $ IO.sendNonparametricQuery pqConnection sql

-- |
-- Parameters and a specification of the parametric query to apply them to.
query :: a -> Query.Query a b -> Session b
query input (Query.Query (Kleisli impl)) =
  Session (impl input)

