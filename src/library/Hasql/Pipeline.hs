module Hasql.Pipeline
  ( Pipeline.Pipeline,
    statement,
  )
where

import Hasql.Engine.Contexts.Pipeline qualified as Pipeline
import Hasql.Engine.Statement qualified as Statement
import Hasql.Platform.Prelude

-- |
-- Execute a statement by providing parameters to it.
statement :: params -> Statement.Statement params result -> Pipeline.Pipeline result
statement params (Statement.Statement sql encoder decoder preparable) =
  Pipeline.statement (encodeUtf8 sql) encoder decoder preparable params
