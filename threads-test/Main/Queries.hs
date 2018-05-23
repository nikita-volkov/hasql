module Main.Queries where

import Rebase.Prelude
import Hasql.Query
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D


selectSleep :: Query Double ()
selectSleep =
  Query sql encoder decoder True
  where
    sql =
      "select pg_sleep($1)"
    encoder =
      E.value E.float8
    decoder =
      D.unit


