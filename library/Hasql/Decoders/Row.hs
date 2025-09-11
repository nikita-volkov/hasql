module Hasql.Decoders.Row where

import Data.ByteString qualified as ByteString
import Data.Text qualified as Text
import Hasql.DecoderCompat qualified as DecoderCompat
import Hasql.Decoders.Value qualified as Value
import Hasql.Errors
import Hasql.LibPq14 qualified as Pq
import Hasql.Prelude hiding (error)
import PostgreSQL.Binary.Decoding qualified as A

newtype Row a
  = Row (ReaderT Env (ExceptT RowError IO) a)
  deriving (Functor, Applicative, Monad)

instance MonadFail Row where
  fail = error . ValueError . fromString

data Env
  = Env Pq.Result Pq.Row Pq.Column Bool (IORef Pq.Column)

-- * Functions

{-# INLINE run #-}
run :: Row a -> (Pq.Result, Pq.Row, Pq.Column, Bool) -> IO (Either (Int, RowError) a)
run (Row impl) (result, row, columnsAmount, integerDatetimes) =
  do
    columnRef <- newIORef 0
    runExceptT (runReaderT impl (Env result row columnsAmount integerDatetimes columnRef)) >>= \case
      Left e -> do
        Pq.Col col <- readIORef columnRef
        -- -1 because succ is applied before the error is returned
        pure $ Left (fromIntegral col - 1, e)
      Right x -> pure $ Right x

{-# INLINE error #-}
error :: RowError -> Row a
error x =
  Row (ReaderT (const (ExceptT (pure (Left x)))))

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE value #-}
value :: Value.Value a -> Row (Maybe a)
value valueDec =
  {-# SCC "value" #-}
  Row
    $ ReaderT
    $ \(Env result row columnsAmount integerDatetimes columnRef) -> ExceptT $ do
      col <- readIORef columnRef
      writeIORef columnRef (succ col)
      if col < columnsAmount
        then do
          -- Get the actual column type for better error reporting
          Pq.Oid actualOidRaw <- Pq.ftype result col
          let actualOid = fromIntegral actualOidRaw
          valueMaybe <- {-# SCC "getvalue'" #-} Pq.getvalue' result row col
          pure
            $ case valueMaybe of
              Nothing ->
                Right Nothing
              Just value -> do
                -- Check for obvious type mismatches before attempting decode
                if isKnownProblemPattern actualOid value
                  then Left (DecoderTypeMismatch actualOid ("Likely type mismatch: found " <> DecoderCompat.oidToTypeName actualOid <> " data"))
                  else do
                    -- Attempt to decode the value
                    let decodeResult = {-# SCC "decode" #-} A.valueParser (Value.run valueDec integerDatetimes) value
                    case decodeResult of
                      Left decodeError ->
                        -- Check if this might be a type mismatch
                        if isLikelyTypeMismatch actualOid decodeError
                          then Left (DecoderTypeMismatch actualOid decodeError)
                          else Left (ValueError decodeError)
                      Right decoded -> Right (Just decoded)
        else pure (Left EndOfInput)
  where
    isKnownProblemPattern :: Word32 -> ByteString -> Bool
    isKnownProblemPattern oid bytes =
      case oid of
        -- INT8 (OID 20) - if the data looks like an 8-byte integer but we're
        -- probably trying to decode it as UUID (16 bytes), this is likely wrong
        20 -> ByteString.length bytes == 8
        -- Add other patterns as needed
        _ -> False

    isLikelyTypeMismatch :: Word32 -> Text -> Bool
    isLikelyTypeMismatch oid errorMsg =
      -- Detect common type mismatch patterns
      case oid of
        20 -> "uuid" `Text.isInfixOf` errorMsg -- INT8 being decoded as UUID
        2950 -> "int" `Text.isInfixOf` errorMsg -- UUID being decoded as INT
        _ -> False

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE nonNullValue #-}
nonNullValue :: Value.Value a -> Row a
nonNullValue valueDec =
  {-# SCC "nonNullValue" #-}
  value valueDec >>= maybe (error UnexpectedNull) pure
