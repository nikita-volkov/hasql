module Hasql.Connection.Config.ConnectionString where

import Data.ByteString qualified as B
import Data.Map.Strict qualified as Map
import Hasql.Connection.Config.ConnectionString.Params qualified as Params
import Hasql.Prelude

type ConnectionString = ByteString

class Constructs a where
  construct :: a -> ConnectionString

fromParams :: Params.Params -> ConnectionString
fromParams =
  B.intercalate " " . fmap renderParam . Map.toList
  where
    renderParam (k, v) = mconcat [k, "=", v]
