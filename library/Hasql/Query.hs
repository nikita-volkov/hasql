module Hasql.Query
(
  ParametricQuery(..),
  NonparametricQuery(..),
)
where

import Hasql.Prelude
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Hasql.Deserialization as Deserialization
import qualified Hasql.Serialization as Serialization


-- |
-- A strictly single-statement query, which can be parameterized and prepared.
-- 
-- SQL template, params serializer, results deserializer and a flag, determining whether it should be prepared.
-- 
type ParametricQuery a b =
  (ByteString, Serialization.Params a, Deserialization.Results b, Bool)

-- |
-- A non-parameterizable and non-preparable query,
-- which however can contain multiple statements.
-- 
-- SQL, results deserializer.
-- 
type NonparametricQuery a =
  (ByteString, Deserialization.Results a)
