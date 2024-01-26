module Hasql.Statement.Function where

import qualified ByteString.StrictBuilder as Builder
import qualified Hasql.Encoders.All as Encoders
import Hasql.Prelude
import qualified Hasql.Statement.Function.SqlBuilder as SqlBuilder

sql :: Text -> Encoders.Params a -> ByteString
sql name encoders =
  Builder.builderBytes
    $ SqlBuilder.sql name
    $ error "TODO: Get size" encoders
