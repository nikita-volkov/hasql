module Hasql.Pipeline
  ( Pipeline.Pipeline,
    statement,
  )
where

import Core.Contexts.ParamsEncoder qualified as ParamsEncoder
import Core.Contexts.Pipeline qualified as Pipeline
import Core.Contexts.ResultsDecoder qualified as ResultsDecoder
import Hasql.Statement qualified as Statement

-- |
-- Execute a statement by providing parameters to it.
statement :: params -> Statement.Statement params result -> Pipeline.Pipeline result
statement
  params
  ( Statement.Statement
      sql
      (ParamsEncoder.unwrap -> paramsEncoder)
      (ResultsDecoder.unwrap -> decoder)
      preparable
    ) =
    Pipeline.statement sql paramsEncoder decoder preparable params
