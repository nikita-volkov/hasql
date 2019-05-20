module Main.Statements where

import Rebase.Prelude
import Hasql.Statement
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D


selectSleep :: Statement Double ()
selectSleep =
  Statement sql encoder decoder True
  where
    sql =
      "select pg_sleep($1)"
    encoder =
      E.param (E.nonNullable (E.primitive E.float8))
    decoder =
      D.unit


