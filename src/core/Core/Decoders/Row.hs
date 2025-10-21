module Core.Decoders.Row where

import Codecs.Encoders.Params qualified as Params
import Core.Errors qualified as Errors
import Core.PqProcedures.SelectTypeInfo qualified as PqProcedures.SelectTypeInfo
import Core.Structures.OidCache qualified as OidCache
import Core.Structures.StatementCache qualified as StatementCache
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Hipq.Roundtrip qualified
import Hipq.RowDecoder qualified
import Platform.Prelude
import Pq qualified

data Row a
  = Row
      (HashSet (Maybe Text, Text))
      ( HashMap (Maybe Text, Text) (Word32, Word32) ->
        Hipq.RowDecoder.RowDecoder a
      )

deriving instance Functor Row

instance Applicative Row where
  pure a = Row HashSet.empty (\_oidCache -> pure a)
  Row lUnknownTypes lDec <*> Row rUnknownTypes rDec =
    Row
      (lUnknownTypes <> rUnknownTypes)
      ( \oidCache ->
          lDec oidCache <*> rDec oidCache
      )

toUnknownTypes :: Row a -> HashSet (Maybe Text, Text)
toUnknownTypes (Row unknownTypes _) = unknownTypes

toDecoder ::
  Row a ->
  HashMap (Maybe Text, Text) (Word32, Word32) ->
  Hipq.RowDecoder.RowDecoder a
toDecoder (Row _unknownTypes decoder) oidCache =
  decoder oidCache
