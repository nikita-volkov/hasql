module Main where

import BasePrelude hiding (assert, isRight, isLeft)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Functor.Contravariant
import Data.Default.Class
import Contravariant.Extras
import qualified Hasql.Query as Query;
import qualified Hasql.Serialization as S
import qualified Hasql.Deserialization as D
import qualified Hasql.Connection as Connection


main =
  do
    connectionEither <- Connection.acquire settings
    case connectionEither of
      Left e -> print e
      Right connection -> do
        result <- Connection.executeParametricQuery connection sumParametricQuery (1, 2)
        print result
  where
    settings =
      Connection.ParametricSettings "localhost" 5432 "postgres" "" "postgres"
    sumParametricQuery =
      (,,,) template serializer deserializer True
      where
        template =
          "SELECT $1 + $2"
        serializer =
          contramap fst (S.value S.int8) <>
          contramap snd (S.value S.int8)
        deserializer =
          D.result (D.singleRow (D.value D.int8))



-- * Model
-------------------------


data Account =
  Account { 
    email :: Text, 
    password :: ByteString, 
    firstName :: Text, 
    lastName :: Text
  }


-- * Queries
-------------------------


updateMenu :: Query.ParametricQuery (Text, Int64) Int64
updateMenu =
  (,,,) template serializer deserializer True
  where
    template =
      "UPDATE menu SET title = $1 WHERE id = $2"
    serializer =
      contrazip2 (S.value S.text)
                 (S.value S.int8)
    deserializer =
      D.result (D.rowsAffected)

accountByEmail :: Query.ParametricQuery Text (Maybe (Int64, Account))
accountByEmail =
  (,,,) template serializer deserializer True
  where
    template =
      "SELECT id, email, password, first_name, last_name \
      \FROM account WHERE email = $1"
    serializer =
      S.value S.text
    deserializer =
      D.result (D.maybeRow (identifiedDeserializer accountDeserializer))

insertAccount :: Query.ParametricQuery Account Int64
insertAccount =
  (,,,) template serializer deserializer True
  where
    template =
      "INSERT INTO account (email, password, first_name, last_name) \
      \VALUES ($1, $2, $3, $4) \
      \RETURNING id"
    serializer =
      accountSerializer
    deserializer =
      D.result (D.singleRow idDeserializer)


-- * Deserializers
-------------------------


idDeserializer :: D.Row Int64
idDeserializer =
  D.value D.int8

accountDeserializer :: D.Row Account
accountDeserializer =
  liftM4 Account (D.value def) (D.value def) (D.value def) (D.value def)

identifiedDeserializer :: D.Row a -> D.Row (Int64, a)
identifiedDeserializer aDeserializer =
  liftM2 (,) idDeserializer aDeserializer


-- * Serializers
-------------------------


accountSerializer :: S.Params Account
accountSerializer =
  contramap (\(Account a b c d) -> (a, b, c, d)) $
  contrazip4 (S.value S.text)
             (S.value S.bytea)
             (S.value S.text)
             (S.value S.text)

