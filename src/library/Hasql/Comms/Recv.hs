module Hasql.Comms.Recv
  ( Recv,
    singleResult,
    allResults,
    toHandler,
    Error (..),
  )
where

import Hasql.Comms.ResultDecoder qualified as ResultDecoder
import Hasql.Driver.Interface qualified as Interface
import Hasql.Platform.Prelude

newtype Recv conn context a
  = Recv (conn -> IO (Either (Error context) a))
  deriving stock (Functor)

instance Applicative (Recv conn context) where
  {-# INLINE pure #-}
  pure x = Recv \_ -> pure (Right x)
  {-# INLINE (<*>) #-}
  Recv recv1 <*> Recv recv2 =
    Recv \cs -> do
      ef <- recv1 cs
      eg <- recv2 cs
      pure (ef <*> eg)

instance Bifunctor (Recv conn) where
  {-# INLINE bimap #-}
  bimap f g (Recv recv) = Recv (fmap (bimap (fmap f) g) . recv)

toHandler :: Recv conn context a -> conn -> IO (Either (Error context) a)
toHandler (Recv recv) = recv

-- | Exactly one result.
singleResult :: Interface.Driver conn result -> context -> ResultDecoder.ResultDecoder a -> Recv conn context a
singleResult drv context handler = Recv \connection -> runExceptT do
  firstResult <- ExceptT do
    mResult <- Interface.driverGetResult drv connection
    case mResult of
      Nothing -> do
        errorMessage <- Interface.driverErrorMessage drv connection
        pure (Left (NoResultsError context errorMessage))
      Just r -> pure (Right r)
  ExceptT do
    mExtra <- Interface.driverGetResult drv connection
    case mExtra of
      Nothing -> pure (Right ())
      Just _ -> pure (Left (TooManyResultsError context 1))
  ExceptT do
    decoded <- ResultDecoder.toHandler handler (Interface.driverResult drv) firstResult
    pure (first (ResultError context 0) decoded)

-- | Consume all results from a multi-statement query (e.g., scripts).
allResults :: Interface.Driver conn result -> context -> ResultDecoder.ResultDecoder a -> Recv conn context ()
allResults drv context handler = Recv \connection -> do
  let loop resultIndex maybeError = do
        result <- Interface.driverGetResult drv connection
        case result of
          Nothing -> pure maybeError
          Just result -> do
            decodedResult <- ResultDecoder.toHandler handler (Interface.driverResult drv) result
            case decodedResult of
              Left err ->
                -- Continue consuming results even after error to clean up connection
                loop (resultIndex + 1) (Just (ResultError context resultIndex err))
              Right _ ->
                loop (resultIndex + 1) maybeError
  errorOrUnit <- loop 0 Nothing
  pure (maybe (Right ()) Left errorOrUnit)

-- * Errors

data Error context
  = ResultError
      context
      -- | Offset of the result in the series.
      Int
      -- | Underlying error.
      ResultDecoder.Error
  | NoResultsError
      context
      -- | Details about the error. Possibly empty.
      (Maybe ByteString)
  | TooManyResultsError
      context
      -- | Expected count.
      Int
  deriving stock (Show, Eq, Functor)
