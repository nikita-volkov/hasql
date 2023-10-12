module Hasql.Private.Decoders.Result where

import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import qualified Data.ByteString as ByteString
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MutableVector
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Hasql.Private.Decoders.Row as Row
import Hasql.Private.Errors
import Hasql.Private.Prelude hiding (many, maybe)
import qualified Hasql.Private.Prelude as Prelude

newtype Result a
  = Result (ReaderT (Bool, LibPQ.Result) (ExceptT ResultError IO) a)
  deriving (Functor, Applicative, Monad)

{-# INLINE run #-}
run :: Result a -> (Bool, LibPQ.Result) -> IO (Either ResultError a)
run (Result reader) env =
  runExceptT (runReaderT reader env)

{-# INLINE noResult #-}
noResult :: Result ()
noResult =
  checkExecStatus $ \case
    LibPQ.CommandOk -> True
    LibPQ.TuplesOk -> True
    _ -> False

{-# INLINE rowsAffected #-}
rowsAffected :: Result Int64
rowsAffected =
  do
    checkExecStatus $ \case
      LibPQ.CommandOk -> True
      _ -> False
    Result
      $ ReaderT
      $ \(_, result) ->
        ExceptT
          $ LibPQ.cmdTuples result
          & fmap cmdTuplesReader
  where
    cmdTuplesReader =
      notNothing >=> notEmpty >=> decimal
      where
        notNothing =
          Prelude.maybe (Left (UnexpectedResult "No bytes")) Right
        notEmpty bytes =
          if ByteString.null bytes
            then Left (UnexpectedResult "Empty bytes")
            else Right bytes
        decimal bytes =
          mapLeft (\m -> UnexpectedResult ("Decimal parsing failure: " <> fromString m))
            $ Attoparsec.parseOnly (Attoparsec.decimal <* Attoparsec.endOfInput) bytes

{-# INLINE checkExecStatus #-}
checkExecStatus :: (LibPQ.ExecStatus -> Bool) -> Result ()
checkExecStatus predicate =
  {-# SCC "checkExecStatus" #-}
  do
    status <- Result $ ReaderT $ \(_, result) -> lift $ LibPQ.resultStatus result
    unless (predicate status) $ do
      case status of
        LibPQ.BadResponse -> serverError
        LibPQ.NonfatalError -> serverError
        LibPQ.FatalError -> serverError
        LibPQ.EmptyQuery -> return ()
        _ -> Result $ lift $ ExceptT $ pure $ Left $ UnexpectedResult $ "Unexpected result status: " <> (fromString $ show status)

{-# INLINE serverError #-}
serverError :: Result ()
serverError =
  Result
    $ ReaderT
    $ \(_, result) -> ExceptT $ do
      code <-
        fmap fold
          $ LibPQ.resultErrorField result LibPQ.DiagSqlstate
      message <-
        fmap fold
          $ LibPQ.resultErrorField result LibPQ.DiagMessagePrimary
      detail <-
        LibPQ.resultErrorField result LibPQ.DiagMessageDetail
      hint <-
        LibPQ.resultErrorField result LibPQ.DiagMessageHint
      position <-
        parsePosition <$> LibPQ.resultErrorField result LibPQ.DiagStatementPosition
      pure $ Left $ ServerError code message detail hint position
  where
    parsePosition = \case
      Nothing -> Nothing
      Just pos ->
        case Attoparsec.parseOnly (Attoparsec.decimal <* Attoparsec.endOfInput) pos of
          Right pos -> Just pos
          _ -> Nothing

{-# INLINE maybe #-}
maybe :: Row.Row a -> Result (Maybe a)
maybe rowDec =
  do
    checkExecStatus $ \case
      LibPQ.TuplesOk -> True
      _ -> False
    Result
      $ ReaderT
      $ \(integerDatetimes, result) -> ExceptT $ do
        maxRows <- LibPQ.ntuples result
        case maxRows of
          0 -> return (Right Nothing)
          1 -> do
            maxCols <- LibPQ.nfields result
            let fromRowError (col, err) = RowError 0 col err
            fmap (fmap Just . mapLeft fromRowError) $ Row.run rowDec (result, 0, maxCols, integerDatetimes)
          _ -> return (Left (UnexpectedAmountOfRows (rowToInt maxRows)))
  where
    rowToInt (LibPQ.Row n) =
      fromIntegral n
    intToRow =
      LibPQ.Row . fromIntegral

{-# INLINE single #-}
single :: Row.Row a -> Result a
single rowDec =
  do
    checkExecStatus $ \case
      LibPQ.TuplesOk -> True
      _ -> False
    Result
      $ ReaderT
      $ \(integerDatetimes, result) -> ExceptT $ do
        maxRows <- LibPQ.ntuples result
        case maxRows of
          1 -> do
            maxCols <- LibPQ.nfields result
            let fromRowError (col, err) = RowError 0 col err
            fmap (mapLeft fromRowError) $ Row.run rowDec (result, 0, maxCols, integerDatetimes)
          _ -> return (Left (UnexpectedAmountOfRows (rowToInt maxRows)))
  where
    rowToInt (LibPQ.Row n) =
      fromIntegral n
    intToRow =
      LibPQ.Row . fromIntegral

{-# INLINE vector #-}
vector :: Row.Row a -> Result (Vector a)
vector rowDec =
  do
    checkExecStatus $ \case
      LibPQ.TuplesOk -> True
      _ -> False
    Result
      $ ReaderT
      $ \(integerDatetimes, result) -> ExceptT $ do
        maxRows <- LibPQ.ntuples result
        maxCols <- LibPQ.nfields result
        mvector <- MutableVector.unsafeNew (rowToInt maxRows)
        failureRef <- newIORef Nothing
        forMFromZero_ (rowToInt maxRows) $ \rowIndex -> do
          rowResult <- Row.run rowDec (result, intToRow rowIndex, maxCols, integerDatetimes)
          case rowResult of
            Left !(!colIndex, !x) -> writeIORef failureRef (Just (RowError rowIndex colIndex x))
            Right !x -> MutableVector.unsafeWrite mvector rowIndex x
        readIORef failureRef >>= \case
          Nothing -> Right <$> Vector.unsafeFreeze mvector
          Just x -> pure (Left x)
  where
    rowToInt (LibPQ.Row n) =
      fromIntegral n
    intToRow =
      LibPQ.Row . fromIntegral

{-# INLINE foldl #-}
foldl :: (a -> b -> a) -> a -> Row.Row b -> Result a
foldl step init rowDec =
  {-# SCC "foldl" #-}
  do
    checkExecStatus $ \case
      LibPQ.TuplesOk -> True
      _ -> False
    Result
      $ ReaderT
      $ \(integerDatetimes, result) ->
        ExceptT
          $ {-# SCC "traversal" #-}
          do
            maxRows <- LibPQ.ntuples result
            maxCols <- LibPQ.nfields result
            accRef <- newIORef init
            failureRef <- newIORef Nothing
            forMFromZero_ (rowToInt maxRows) $ \rowIndex -> do
              rowResult <- Row.run rowDec (result, intToRow rowIndex, maxCols, integerDatetimes)
              case rowResult of
                Left !(!colIndex, !x) -> writeIORef failureRef (Just (RowError rowIndex colIndex x))
                Right !x -> modifyIORef' accRef (\acc -> step acc x)
            readIORef failureRef >>= \case
              Nothing -> Right <$> readIORef accRef
              Just x -> pure (Left x)
  where
    rowToInt (LibPQ.Row n) =
      fromIntegral n
    intToRow =
      LibPQ.Row . fromIntegral

{-# INLINE foldr #-}
foldr :: (b -> a -> a) -> a -> Row.Row b -> Result a
foldr step init rowDec =
  {-# SCC "foldr" #-}
  do
    checkExecStatus $ \case
      LibPQ.TuplesOk -> True
      _ -> False
    Result
      $ ReaderT
      $ \(integerDatetimes, result) -> ExceptT $ do
        maxRows <- LibPQ.ntuples result
        maxCols <- LibPQ.nfields result
        accRef <- newIORef init
        failureRef <- newIORef Nothing
        forMToZero_ (rowToInt maxRows) $ \rowIndex -> do
          rowResult <- Row.run rowDec (result, intToRow rowIndex, maxCols, integerDatetimes)
          case rowResult of
            Left !(!colIndex, !x) -> writeIORef failureRef (Just (RowError rowIndex colIndex x))
            Right !x -> modifyIORef accRef (\acc -> step x acc)
        readIORef failureRef >>= \case
          Nothing -> Right <$> readIORef accRef
          Just x -> pure (Left x)
  where
    rowToInt (LibPQ.Row n) =
      fromIntegral n
    intToRow =
      LibPQ.Row . fromIntegral
