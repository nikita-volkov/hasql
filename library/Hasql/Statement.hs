module Hasql.Statement
where

import Hasql.Private.Prelude
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders


-- |
-- A specification of a strictly single-statement query, which can be parameterized and prepared.
-- 
-- Consists of the following:
-- 
-- * SQL template,
-- * params encoder,
-- * result decoder,
-- * a flag, determining whether it should be prepared.
-- 
-- The SQL template must be formatted according to Postgres' standard,
-- with any non-ASCII characters of the template encoded using UTF-8.
-- According to the format,
-- parameters must be referred to using the positional notation, as in the following:
-- @$1@, @$2@, @$3@ and etc.
-- Those references must be used to refer to the values of the 'Encoders.Params' encoder.
-- 
-- Following is an example of the declaration of a prepared statement with its associated codecs.
-- 
-- @
-- selectSum :: Hasql.Statement.'Statement' (Int64, Int64) Int64
-- selectSum =
--   Hasql.Statement.'Statement' sql encoder decoder True
--   where
--     sql =
--       "select ($1 + $2)"
--     encoder =
--       'contramap' 'fst' (Hasql.Encoders.'Hasql.Encoders.param' Hasql.Encoders.'Hasql.Encoders.int8') '<>'
--       'contramap' 'snd' (Hasql.Encoders.'Hasql.Encoders.param' Hasql.Encoders.'Hasql.Encoders.int8')
--     decoder =
--       Hasql.Decoders.'Hasql.Decoders.singleRow' (Hasql.Decoders.'Hasql.Decoders.column' Hasql.Decoders.'Hasql.Decoders.int8')
-- @
-- 
-- The statement above accepts a product of two parameters of type 'Int64'
-- and produces a single result of type 'Int64'.
-- 
data Statement a b =
  Statement ByteString (Encoders.Params a) (Decoders.Result b) Bool

instance Functor (Statement a) where
  {-# INLINE fmap #-}
  fmap = rmap

instance Profunctor Statement where
  {-# INLINE dimap #-}
  dimap f1 f2 (Statement template encoder decoder preparable) =
    Statement template (contramap f1 encoder) (fmap f2 decoder) preparable

data MultiParamStatement a b =
  MultiParamStatement ByteString [Encoders.Params a] (Decoders.Result b) Bool

instance Functor (MultiParamStatement a) where
  {-# INLINE fmap #-}
  fmap = rmap
instance Profunctor MultiParamStatement where
  {-# INLINE dimap #-}
  dimap f1 f2 (MultiParamStatement template encoders decoder preparable) =
    MultiParamStatement template (contramap f1 <$> encoders) (fmap f2 decoder) preparable
