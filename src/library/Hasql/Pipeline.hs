module Hasql.Pipeline
  ( Pipeline.Pipeline,
    statement,
  )
where

import Core.Contexts.Pipeline qualified as Pipeline
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Statement qualified as Statement

-- |
-- Execute a statement by providing parameters to it.
statement :: params -> Statement.Statement params result -> Pipeline.Pipeline result
statement params (Statement.Statement sql (Encoders.Params paramsEncoder) (Decoders.Result decoder) preparable) =
  Pipeline.statement sql paramsEncoder decoder preparable params
