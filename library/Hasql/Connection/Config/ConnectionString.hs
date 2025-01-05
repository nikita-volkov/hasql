module Hasql.Connection.Config.ConnectionString where

import Data.ByteString qualified as B
import Data.Map.Strict qualified as Map
import Data.Text.Encoding qualified
import Hasql.Connection.Config.ConnectionString.Params qualified as Params
import Hasql.Prelude

type ConnectionString = ByteString

class Constructs a where
  construct :: a -> ConnectionString

fromText :: Text -> ConnectionString
fromText = Data.Text.Encoding.encodeUtf8

fromParams :: Params.Params -> ConnectionString
fromParams =
  B.intercalate " " . fmap renderParam . Map.toList
  where
    renderParam (k, v) = mconcat [k, "=", v]
