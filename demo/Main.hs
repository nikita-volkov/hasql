module Main where

import BasePrelude hiding (assert, isRight, isLeft)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Functor.Contravariant
import Data.Default.Class
import Contravariant.Extras
import qualified Hasql as H
import qualified Hasql.Serialization as HS
import qualified Hasql.Deserialization as HD


main =
  do
    connectionEither <- H.connect settings
    case connectionEither of
      Left e -> print e
      Right connection -> do
        result <- H.query connection sumQuery (1, 2)
        print result
  where
    settings =
      H.ParametricSettings "localhost" 5432 "postgres" "" "postgres"
    sumQuery =
      (,,,) template serializer deserializer True
      where
        template =
          "SELECT $1 + $2"
        serializer =
          contramap fst (HS.value HS.int8) <>
          contramap snd (HS.value HS.int8)
        deserializer =
          HD.singleRow (HD.value HD.int8)



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


updateMenu :: H.Query (Text, Int64) Int64
updateMenu =
  (,,,) template serializer deserializer True
  where
    template =
      "UPDATE menu SET title = $1 WHERE id = $2"
    serializer =
      contrazip2 (HS.value HS.text)
                 (HS.value HS.int8)
    deserializer =
      HD.rowsAffected

accountByEmail :: H.Query Text (Maybe (Int64, Account))
accountByEmail =
  (,,,) template serializer deserializer True
  where
    template =
      "SELECT id, email, password, first_name, last_name \
      \FROM account WHERE email = $1"
    serializer =
      HS.value HS.text
    deserializer =
      HD.maybeRow (identifiedDeserializer accountDeserializer)

insertAccount :: H.Query Account Int64
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
      HD.singleRow idDeserializer


-- * Deserializers
-------------------------


idDeserializer :: HD.Row Int64
idDeserializer =
  HD.value HD.int8

accountDeserializer :: HD.Row Account
accountDeserializer =
  liftM4 Account (HD.value def) (HD.value def) (HD.value def) (HD.value def)

identifiedDeserializer :: HD.Row a -> HD.Row (Int64, a)
identifiedDeserializer aDeserializer =
  liftM2 (,) idDeserializer aDeserializer


-- * Serializers
-------------------------


accountSerializer :: HS.Params Account
accountSerializer =
  contramap (\(Account a b c d) -> (a, b, c, d)) $
  contrazip4 (HS.value HS.text)
             (HS.value HS.bytea)
             (HS.value HS.text)
             (HS.value HS.text)

