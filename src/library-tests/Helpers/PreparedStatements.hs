-- |
-- Observation of what the server actually holds prepared on a connection.
module Helpers.PreparedStatements
  ( countPrepared,
    isPrepared,
  )
where

import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Prelude

-- |
-- Amount of statements the server currently holds prepared, not counting the
-- probe itself.
--
-- Each probe carries a fresh marker, so no two probes are the same statement
-- and the driver never sees one often enough to prepare it. That keeps the
-- observation from disturbing what it observes — as long as the connection
-- under test has a threshold above one.
countPrepared :: Session.Session Int32
countPrepared = do
  marker <- liftIO Scripts.generateSymname
  Session.statement () (countPreparedStatement marker)

-- |
-- Whether the server holds a statement whose text contains the given marker.
isPrepared :: Text -> Session.Session Bool
isPrepared marker = do
  probeMarker <- liftIO Scripts.generateSymname
  count <- Session.statement ("%" <> marker <> "%") (countPreparedLikeStatement probeMarker)
  pure (count > 0)

countPreparedStatement :: Text -> Statement.Statement () Int32
countPreparedStatement marker =
  Statement.statement
    ( "select count(*)::int4 from pg_prepared_statements \
      \where statement not like '%hasql_probe%' -- hasql_probe "
        <> marker
    )
    mempty
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))

countPreparedLikeStatement :: Text -> Statement.Statement Text Int32
countPreparedLikeStatement marker =
  Statement.statement
    ( "select count(*)::int4 from pg_prepared_statements \
      \where statement like $1 and statement not like '%hasql_probe%' -- hasql_probe "
        <> marker
    )
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
