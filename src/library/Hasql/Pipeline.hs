module Hasql.Pipeline
  ( Pipeline.Pipeline,
    statement,
  )
where

import Hasql.Engine.Contexts.Pipeline qualified as Pipeline
import Hasql.Engine.Statement qualified as Statement

-- |
-- Execute a statement by providing parameters to it.
statement :: params -> Statement.Statement params result -> Pipeline.Pipeline result
statement params stmt = Pipeline.statement stmt params
