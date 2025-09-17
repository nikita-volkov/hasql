module Core.Contexts.ResultConsumer
  ( ResultConsumer,
    Handler,
    toHandler,
    fromHandler,
    ok,
    pipelineSync,
    rowsAffected,
    checkExecStatus,
    columnOids,

    -- * Simple constructors (aliases for compatibility)
    noResult,

    -- * Higher-level decoders
    maybe,
    single,
    vector,
    foldl,
    foldr,

    -- * Refinement
    refine,

    -- * Classes
    Wraps (..),
  )
where

import Core.Contexts.Recv qualified as Recv
import Core.Contexts.RowDecoder qualified as Row
import Core.Errors
import Data.Attoparsec.ByteString.Char8 qualified as Attoparsec
import Data.ByteString qualified as ByteString
import Data.Vector qualified as Vector
import Data.Vector.Mutable qualified as MutableVector
import Platform.Prelude hiding (foldl, foldr, maybe)
import Platform.Prelude qualified as Prelude
import Pq qualified

-- | Result consumption context, for consuming a single result from a sequence of results returned by the server.
newtype ResultConsumer a
  = ResultConsumer (Pq.Result -> IO (Either ResultError a))
  deriving
    (Functor, Applicative, Monad, MonadError ResultError, MonadReader Pq.Result)
    via (ReaderT Pq.Result (ExceptT ResultError IO))

instance Filterable ResultConsumer where
  {-# INLINE mapMaybe #-}
  mapMaybe fn =
    refine (Prelude.maybe (Left "Invalid result") Right . fn)

instance Recv.HandlesResult ResultConsumer where
  handleResult = toHandler

-- * Relations

-- ** Handler

type Handler a = Pq.Result -> IO (Either ResultError a)

toHandler :: ResultConsumer a -> Handler a
toHandler (ResultConsumer handler) =
  handler

fromHandler :: Handler a -> ResultConsumer a
fromHandler handler =
  ResultConsumer handler

-- * Construction

{-# INLINE ok #-}
ok :: ResultConsumer ()
ok = checkExecStatus [Pq.CommandOk, Pq.TuplesOk]

{-# INLINE pipelineSync #-}
pipelineSync :: ResultConsumer ()
pipelineSync = checkExecStatus [Pq.PipelineSync]

{-# INLINE rowsAffected #-}
rowsAffected :: ResultConsumer Int64
rowsAffected = do
  checkExecStatus [Pq.CommandOk]
  ResultConsumer \result -> do
    cmdTuplesReader <$> Pq.cmdTuples result
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
          first
            ( \m ->
                UnexpectedResult
                  ("Decimal parsing failure: " <> fromString m)
            )
            ( Attoparsec.parseOnly
                (Attoparsec.decimal <* Attoparsec.endOfInput)
                bytes
            )

{-# INLINE checkExecStatus #-}
checkExecStatus :: [Pq.ExecStatus] -> ResultConsumer ()
checkExecStatus expectedList = do
  status <- ResultConsumer \result -> Right <$> Pq.resultStatus result
  unless (elem status expectedList) $ do
    case status of
      Pq.BadResponse -> serverError
      Pq.NonfatalError -> serverError
      Pq.FatalError -> serverError
      Pq.EmptyQuery -> return ()
      _ ->
        throwError
          ( UnexpectedResult
              ("Unexpected result status: " <> fromString (show status) <> ". Expecting one of the following: " <> fromString (show expectedList))
          )

{-# INLINE serverError #-}
serverError :: ResultConsumer ()
serverError =
  ResultConsumer \result -> do
    code <-
      fold <$> Pq.resultErrorField result Pq.DiagSqlstate
    message <-
      fold <$> Pq.resultErrorField result Pq.DiagMessagePrimary
    detail <-
      Pq.resultErrorField result Pq.DiagMessageDetail
    hint <-
      Pq.resultErrorField result Pq.DiagMessageHint
    position <-
      parsePosition <$> Pq.resultErrorField result Pq.DiagStatementPosition
    pure $ Left $ ServerError code message detail hint position
  where
    parsePosition = \case
      Nothing -> Nothing
      Just pos ->
        case Attoparsec.parseOnly (Attoparsec.decimal <* Attoparsec.endOfInput) pos of
          Right pos -> Just pos
          _ -> Nothing

-- | Get the OIDs of all columns in the current result.
{-# INLINE columnOids #-}
columnOids :: ResultConsumer [Pq.Oid]
columnOids = ResultConsumer \result -> do
  columnsAmount <- Pq.nfields result
  let Pq.Col count = columnsAmount
  oids <- forM [0 .. count - 1] $ \colIndex ->
    Pq.ftype result (Pq.Col colIndex)
  pure (Right oids)

-- * Simple constructors (aliases for compatibility)

{-# INLINE noResult #-}
noResult :: ResultConsumer ()
noResult = ok

-- * Higher-level decoders

{-# INLINE checkCompatibility #-}
checkCompatibility :: Row.RowDecoder a -> ResultConsumer ()
checkCompatibility rowDec =
  fromHandler \result ->
    Row.toCompatibilityCheck rowDec result

{-# INLINE maybe #-}
maybe :: Row.RowDecoder a -> ResultConsumer (Maybe a)
maybe rowDec =
  do
    checkExecStatus [Pq.TuplesOk]
    checkCompatibility rowDec
    ResultConsumer
      $ \result -> do
        maxRows <- Pq.ntuples result
        case maxRows of
          0 -> return (Right Nothing)
          1 -> do
            maxCols <- Pq.nfields result
            result <- Row.toDecoder rowDec 0 maxCols result
            pure (fmap Just result)
          _ -> return (Left (UnexpectedAmountOfRows (rowToInt maxRows)))
  where
    rowToInt (Pq.Row n) =
      fromIntegral n

{-# INLINE single #-}
single :: Row.RowDecoder a -> ResultConsumer a
single rowDec =
  do
    checkExecStatus [Pq.TuplesOk]
    checkCompatibility rowDec
    ResultConsumer
      $ \result -> do
        maxRows <- Pq.ntuples result
        case maxRows of
          1 -> do
            maxCols <- Pq.nfields result
            Row.toDecoder rowDec 0 maxCols result
          _ -> return (Left (UnexpectedAmountOfRows (rowToInt maxRows)))
  where
    rowToInt (Pq.Row n) =
      fromIntegral n

{-# INLINE vector #-}
vector :: Row.RowDecoder a -> ResultConsumer (Vector a)
vector rowDec =
  do
    checkExecStatus [Pq.TuplesOk]
    checkCompatibility rowDec
    ResultConsumer
      $ \result -> do
        maxRows <- Pq.ntuples result
        maxCols <- Pq.nfields result
        mvector <- MutableVector.unsafeNew (rowToInt maxRows)
        failureRef <- newIORef Nothing
        forMFromZero_ (rowToInt maxRows) $ \rowIndex -> do
          rowResult <- Row.toDecoder rowDec (intToRow rowIndex) maxCols result
          case rowResult of
            Left !err -> writeIORef failureRef (Just err)
            Right !x -> MutableVector.unsafeWrite mvector rowIndex x
        readIORef failureRef >>= \case
          Nothing -> Right <$> Vector.unsafeFreeze mvector
          Just x -> pure (Left x)
  where
    rowToInt (Pq.Row n) =
      fromIntegral n
    intToRow =
      Pq.Row . fromIntegral

{-# INLINE foldl #-}
foldl :: (a -> b -> a) -> a -> Row.RowDecoder b -> ResultConsumer a
foldl step init rowDec =
  {-# SCC "foldl" #-}
  do
    checkExecStatus [Pq.TuplesOk]
    checkCompatibility rowDec
    ResultConsumer
      $ \result ->
        {-# SCC "traversal" #-}
        do
          maxRows <- Pq.ntuples result
          maxCols <- Pq.nfields result
          accRef <- newIORef init
          failureRef <- newIORef Nothing
          forMFromZero_ (rowToInt maxRows) $ \rowIndex -> do
            rowResult <- Row.toDecoder rowDec (intToRow rowIndex) maxCols result
            case rowResult of
              Left !err -> writeIORef failureRef (Just err)
              Right !x -> modifyIORef' accRef (\acc -> step acc x)
          readIORef failureRef >>= \case
            Nothing -> Right <$> readIORef accRef
            Just x -> pure (Left x)
  where
    rowToInt (Pq.Row n) =
      fromIntegral n
    intToRow =
      Pq.Row . fromIntegral

{-# INLINE foldr #-}
foldr :: (b -> a -> a) -> a -> Row.RowDecoder b -> ResultConsumer a
foldr step init rowDec =
  {-# SCC "foldr" #-}
  do
    checkExecStatus [Pq.TuplesOk]
    checkCompatibility rowDec
    ResultConsumer
      $ \result -> do
        maxRows <- Pq.ntuples result
        maxCols <- Pq.nfields result
        accRef <- newIORef init
        failureRef <- newIORef Nothing
        forMToZero_ (rowToInt maxRows) $ \rowIndex -> do
          rowResult <- Row.toDecoder rowDec (intToRow rowIndex) maxCols result
          case rowResult of
            Left !err -> writeIORef failureRef (Just err)
            Right !x -> modifyIORef accRef (\acc -> step x acc)
        readIORef failureRef >>= \case
          Nothing -> Right <$> readIORef accRef
          Just x -> pure (Left x)
  where
    rowToInt (Pq.Row n) =
      fromIntegral n
    intToRow =
      Pq.Row . fromIntegral

-- * Refinement

refine :: (a -> Either Text b) -> ResultConsumer a -> ResultConsumer b
refine refiner (ResultConsumer reader) = ResultConsumer
  $ \result -> do
    resultEither <- reader result
    return $ resultEither >>= first UnexpectedResult . refiner

-- * Classes

class Wraps f where
  wrap :: ResultConsumer a -> f a
  unwrap :: f a -> ResultConsumer a
