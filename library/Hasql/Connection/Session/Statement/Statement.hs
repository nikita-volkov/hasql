module Hasql.Connection.Session.Statement.Statement
where

import Hasql.Prelude
import qualified ByteString.StrictBuilder as B
import qualified Data.IntMap.Strict as G
import qualified Data.HashMap.Strict as L
import qualified Hasql.OID.Primitive as D
import qualified Hasql.OID.Array as E
import qualified Hasql.Protocol.Interpreter as H
import qualified Hasql.Protocol.Encoding as P
import qualified Hasql.Protocol.Decoding as S
import qualified Hasql.Client.Model as C
import qualified Hasql.Client.MessagesConsumer as I
import qualified Hasql.Connection.Session.Statement.Decoding.Decoding as M
import qualified Hasql.Connection.Session.Statement.Encoding.Encoding as Q
import qualified Hasql.Connection.Session.Statement.OIDsValidatorBuilder as R
import qualified BinaryParser as J
import qualified Control.Foldl as K
import qualified VectorBuilder.Builder as N
import qualified VectorBuilder.Vector as O


{-|
Parametric statement, which can be prepared.
-}
data Statement params result =
  {-|
  * Template
  * Parameter OIDs
  * Parameters encoder with Integer Datetimes on
  * Parameters encoder with Integer Datetimes off
  * Amount of expected result columns
  * Result column OIDs validator
  * Result collector with Integer Datetimes on
  * Result collector with Integer Datetimes off
  * Prepared flag
  -}
  Statement
    ByteString
    (Vector Word32)
    (params -> B.Builder)
    (params -> B.Builder)
    Int
    (Vector Word32 -> Maybe Text)
    (I.MessagesConsumer result)
    (I.MessagesConsumer result)
    Bool

deriving instance Functor (Statement params)

instance Profunctor Statement where
  {-# INLINE lmap #-}
  lmap fn (Statement template paramOIDs paramBytesBuilder1 paramBytesBuilder2 resultColumnsAmount resultOIDsValidator resultCollector1 resultCollector2 prepared) =
    Statement template paramOIDs (paramBytesBuilder1 . fn) (paramBytesBuilder2 . fn) resultColumnsAmount resultOIDsValidator resultCollector1 resultCollector2 prepared
  {-# INLINE rmap #-}
  rmap fn (Statement template paramOIDs paramBytesBuilder1 paramBytesBuilder2 resultColumnsAmount resultOIDsValidator resultCollector1 resultCollector2 prepared) =
    Statement template paramOIDs paramBytesBuilder1 paramBytesBuilder2 resultColumnsAmount resultOIDsValidator (fn <$> resultCollector1) (fn <$> resultCollector2) prepared
  {-# INLINE dimap #-}
  dimap lfn rfn (Statement template paramOIDs paramBytesBuilder1 paramBytesBuilder2 resultColumnsAmount resultOIDsValidator resultCollector1 resultCollector2 prepared) =
    Statement template paramOIDs (paramBytesBuilder1 . lfn) (paramBytesBuilder2 . lfn) resultColumnsAmount resultOIDsValidator (rfn <$> resultCollector1) (rfn <$> resultCollector2) prepared

statement :: ByteString -> Encoder params -> Decoder result -> Bool -> Statement params result
statement template (Encoder paramOIDsBuilder paramBytesBuilder1 paramBytesBuilder2) (Decoder resultColumnsAmount resultOIDsValidator resultCollector1 resultCollector2) prepared =
  Statement template paramOIDs paramBytesBuilder1 paramBytesBuilder2 resultColumnsAmount resultOIDsValidator resultCollector1 resultCollector2 prepared
  where
    paramOIDs =
      O.build paramOIDsBuilder


-- * Params Encoding
-------------------------

{-|
Encoder of an arbitrary data-structure representing the parameters of a statement.
-}
data Encoder params =
  Encoder (N.Builder Word32) (params -> B.Builder) (params -> B.Builder)

instance Semigroup (Encoder params) where
  {-# INLINE (<>) #-}
  (<>) (Encoder leftOids leftBuilder1 leftBuilder2) (Encoder rightOids rightBuilder1 rightBuilder2) =
    Encoder oids builder1 builder2
    where
      oids =
        leftOids <> rightOids
      builder1 =
        leftBuilder1 <> rightBuilder1
      builder2 =
        leftBuilder2 <> rightBuilder2

instance Monoid (Encoder params) where
  {-# INLINE mempty #-}
  mempty =
    Encoder mempty mempty mempty
  {-# INLINE mappend #-}
  mappend =
    (<>)

instance Contravariant Encoder where
  {-# INLINE contramap #-}
  contramap fn (Encoder oids builder1 builder2) =
    Encoder oids (builder1 . fn) (builder2 . fn)

instance Divisible Encoder where
  {-# INLINE conquer #-}
  conquer =
    mempty
  {-# INLINABLE divide #-}
  divide fn (Encoder leftOids leftBuilder1 leftBuilder2) (Encoder rightOids rightBuilder1 rightBuilder2) =
    Encoder oids builder1 builder2
    where
      oids =
        leftOids <> rightOids
      builder1 =
        mergedBuilder leftBuilder1 rightBuilder1
      builder2 =
        mergedBuilder leftBuilder2 rightBuilder2
      mergedBuilder leftBuilder rightBuilder params =
        case fn params of
          (leftParams, rightParams) ->
            leftBuilder leftParams <>
            rightBuilder rightParams

{-# INLINE param #-}
param :: Q.Param param -> Encoder param
param (Q.Param oid builder1 builder2) =
  Encoder oids paramsBuilder1 paramsBuilder2
  where
    oids =
      N.singleton oid
    paramsBuilder1 =
      paramsBuilder builder1
    paramsBuilder2 =
      paramsBuilder builder2
    paramsBuilder builder =
      P.sizedValue . builder

{-# INLINE nullableParam #-}
nullableParam :: Q.Param param -> Encoder (Maybe param)
nullableParam (Q.Param oid builder1 builder2) =
  Encoder oids paramsBuilder1 paramsBuilder2
  where
    oids =
      N.singleton oid
    paramsBuilder1 =
      paramsBuilder builder1
    paramsBuilder2 =
      paramsBuilder builder2
    paramsBuilder builder =
      P.nullableSizedValue . fmap builder


-- * Result Decoding
-------------------------

{-|
Decoder of the statement result into an arbitrary data-structure.
-}
data Decoder result =
  {-|
  * Amount of columns
  * Validator of result OIDs
  * Backend messages consumer with Integer Datetimes on
  * Backend messages consumer with Integer Datetimes off
  -}
  Decoder Int (Vector Word32 -> Maybe Text) (I.MessagesConsumer result) (I.MessagesConsumer result)

instance Functor Decoder where
  {-# INLINE fmap #-}
  fmap fn (Decoder amount oidsValidator messagesConsumer1 messagesConsumer2) =
    Decoder amount oidsValidator (fn <$> messagesConsumer1) (fn <$> messagesConsumer2)

instance Applicative Decoder where
  {-# INLINE pure #-}
  pure x =
    Decoder 0 (const Nothing) (pure x) (pure x)
  {-# INLINABLE (<*>) #-}
  (<*>) (Decoder leftAmount leftOIDsValidator leftMessagesConsumer1 leftMessagesConsumer2)
        (Decoder rightAmount rightOIDsValidator rightMessagesConsumer1 rightMessagesConsumer2) =
    Decoder amount oidsValidator messagesConsumer1 messagesConsumer2
    where
      amount =
        leftAmount + rightAmount
      oidsValidator =
        (<|>) <$> leftOIDsValidator <*> rightOIDsValidator
      messagesConsumer1 =
        leftMessagesConsumer1 <*> rightMessagesConsumer1
      messagesConsumer2 =
        leftMessagesConsumer2 <*> rightMessagesConsumer2

{-|
The amount of rows affected by the query.
All queries return this result.
-}
{-# INLINE rowsAffected #-}
rowsAffected :: Decoder Int
rowsAffected =
  Decoder 0 (const Nothing) I.rowsAffected I.rowsAffected

{-|
First row of a non-empty result.
Fails if there's no rows.
-}
{-# INLINE row #-}
row :: RowDecoder row -> Decoder row
row (RowDecoder amount oidsValidatorBuilder rowParser1 rowParser2) =
  Decoder amount oidsValidator messagesConsumer1 messagesConsumer2
  where
    oidsValidator =
      R.build oidsValidatorBuilder
    messagesConsumer1 =
      I.row (S.dataRowMessage (const rowParser1))
    messagesConsumer2 =
      I.row (S.dataRowMessage (const rowParser2))

{-|
Essentially, a specification of Map/Reduce over all the rows of the result set.
Can be used to produce all kinds of containers or to implement aggregation algorithms on the client side.
E.g.,

@
rowMaybe :: RowDecoder row -> Decoder (Maybe row)
rowMaybe =
  rowReduction ('K.generalize' 'K.head')

rowVector :: RowDecoder row -> Decoder (Vector row)
rowVector =
  rowReduction ('K.generalize' 'K.vector')

rowList :: RowDecoder row -> Decoder [row]
rowList =
  rowReduction ('K.generalize' 'K.list')
@
-}
{-# INLINE rowReduction #-}
rowReduction :: FoldM IO row reduction -> RowDecoder row -> Decoder reduction
rowReduction fold (RowDecoder amount oidsValidatorBuilder rowParser1 rowParser2) =
  Decoder amount oidsValidator messagesConsumer1 messagesConsumer2
  where
    oidsValidator =
      R.build oidsValidatorBuilder
    messagesConsumer1 =
      I.rowsReduction (S.dataRowMessage (const rowParser1)) fold
    messagesConsumer2 =
      I.rowsReduction (S.dataRowMessage (const rowParser2)) fold

{-|
First row of a possibly empty result set.
-}
{-# INLINE rowMaybe #-}
rowMaybe :: RowDecoder row -> Decoder (Maybe row)
rowMaybe =
  rowReduction (K.generalize K.head)

{-# INLINE rowVector #-}
rowVector :: RowDecoder row -> Decoder (Vector row)
rowVector =
  rowReduction (K.generalize K.vector)

{-|
List of rows. Slower than 'rowRevList'.
-}
{-# INLINE rowList #-}
rowList :: RowDecoder row -> Decoder [row]
rowList =
  rowReduction (K.generalize K.list)

{-|
List in a reverse order. Faster than 'rowList'.
-}
{-# INLINE rowRevList #-}
rowRevList :: RowDecoder row -> Decoder [row]
rowRevList =
  rowReduction (K.generalize K.revList)

{-# INLINE rowHashMap #-}
rowHashMap :: (Eq key, Hashable key) => RowDecoder (key, value) -> Decoder (HashMap key value)
rowHashMap =
  rowReduction (K.generalize K.hashMap)

{-|
Parser of a row.
-}
data RowDecoder row =
  {-|
  * Amount of columns
  * Builder of validator of result OIDs
  * DataRow message payload parser with Integer Datetimes on
  * DataRow message payload parser with Integer Datetimes off
  -}
  RowDecoder Int R.Builder (J.BinaryParser row) (J.BinaryParser row)

deriving instance Functor RowDecoder

instance Applicative RowDecoder where
  {-# INLINE pure #-}
  pure x =
    RowDecoder 0 mempty (pure x) (pure x)
  {-# INLINABLE (<*>) #-}
  (<*>) (RowDecoder leftAmount leftOIDsValidatorBuilder leftRowParser1 leftRowParser2)
        (RowDecoder rightAmount rightOIDsValidatorBuilder rightRowParser1 rightRowParser2) =
    RowDecoder amount oidsValidatorBuilder rowParser1 rowParser2
    where
      amount =
        leftAmount + rightAmount
      oidsValidatorBuilder =
        leftOIDsValidatorBuilder <> rightOIDsValidatorBuilder
      rowParser1 =
        leftRowParser1 <*> rightRowParser1
      rowParser2 =
        leftRowParser2 <*> rightRowParser2

column :: M.Value column -> RowDecoder column
column (M.Value oidValidator parser1 parser2) =
  RowDecoder 1 (R.validator oidValidator) rowParser1 rowParser2
  where
    rowParser1 =
      S.sizedValue parser1
    rowParser2 =
      S.sizedValue parser2

nullableColumn :: M.Value column -> RowDecoder (Maybe column)
nullableColumn (M.Value oidValidator parser1 parser2) =
  RowDecoder 1 (R.validator oidValidator) rowParser1 rowParser2
  where
    rowParser1 =
      S.nullableSizedValue parser1
    rowParser2 =
      S.nullableSizedValue parser2
