module Hasql.Statement.Function where

import qualified ByteString.StrictBuilder as Builder
import qualified Hasql.Encoders.All as Encoders
import qualified Hasql.Encoders.Params as Encoders.Params
import Hasql.Prelude
import qualified Hasql.Statement.Function.SqlBuilder as SqlBuilder

sql :: Text -> Encoders.Params a -> ByteString
sql name (Encoders.Params (Encoders.Params.Params size _ _ _)) =
  Builder.builderBytes
    $ SqlBuilder.sql name
    $ size
