module Hasql.Pipeline
  ( Pipeline,
    statement,
  )
where

import Hasql.Engine.Contexts.Pipeline qualified as Pipeline
import Hasql.Engine.Statement qualified as Statement
import Hasql.Platform.Prelude
import Hasql.Pq qualified as Pq

-- | A pipeline bound to the libpq connection type.
type Pipeline a = Pipeline.Pipeline Pq.Connection a

-- |
-- Execute a statement by providing parameters to it.
statement :: params -> Statement.Statement params result -> Pipeline result
statement params (Statement.Statement sql encoder decoder preparable) =
  Pipeline.statement (encodeUtf8 sql) encoder decoder preparable params
