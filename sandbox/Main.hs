module Main where

import Prelude
import qualified Hasql.Connection as A
import qualified Hasql.Connection.Session as J
import qualified Hasql.Connection.Session.Statement as K
import qualified Hasql.Connection.Session.Statement.Encoding as L
import qualified Hasql.Connection.Session.Statement.Decoding as M
import qualified Data.Vector as H
import qualified Control.Foldl as I


main =
  join $ fmap print $ runExceptT $ do
    connection <- ExceptT (A.acquire "localhost" Nothing "postgres" Nothing Nothing)
    ExceptT (A.use connection session)
  where
    session =
      J.batch batch1
      where
        batch1 =
          (,) <$> J.statement stmt1 "ABC" <*> J.statement stmt1 "abc"
          where
            stmt1 =
              K.statement "select unnest(array[$1,'b','c','d'])" encoder decoder True
              where
                encoder =
                  K.param (L.primitive L.text)
                decoder =
                  K.rowList (K.column M.text)
