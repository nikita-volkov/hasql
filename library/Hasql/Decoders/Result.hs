module Hasql.Decoders.Result where

import Data.Vector qualified as Vector
import Data.Vector.Mutable qualified as MutableVector
import Hasql.Contexts.ResultConsumer qualified as ResultConsumer
import Hasql.Decoders.Row qualified as Row
import Hasql.Errors
import Hasql.LibPq14 qualified as Pq
import Hasql.Prelude hiding (many, maybe)

newtype Result a
  = Result (ReaderT (Bool, Pq.Result) (ExceptT ResultError IO) a)
  deriving (Functor, Applicative, Monad)

-- * Relations

-- ** ResultConsumerByIdt

type ResultConsumerByIdt a = Bool -> ResultConsumer.ResultConsumer a

toResultConsumerByIdt :: Result a -> Bool -> ResultConsumer.ResultConsumer a
toResultConsumerByIdt (Result reader) idt =
  ResultConsumer.fromHandler \result -> do
    runExceptT (runReaderT reader (idt, result))

fromResultConsumerByIdt :: ResultConsumerByIdt a -> Result a
fromResultConsumerByIdt resultConsumerByIdt =
  fromHandler \idt result ->
    ResultConsumer.toHandler (resultConsumerByIdt idt) result

-- ** ResultConsumer

type ResultConsumer a = ResultConsumer.ResultConsumer a

fromResultConsumer :: ResultConsumer a -> Result a
fromResultConsumer handler =
  fromHandler \_idt result ->
    ResultConsumer.toHandler handler result

-- ** Handler

type Handler a = Bool -> Pq.Result -> IO (Either ResultError a)

{-# INLINE toHandler #-}
toHandler :: Result a -> Handler a
toHandler (Result reader) idt result =
  runExceptT (runReaderT reader (idt, result))

fromHandler :: Handler a -> Result a
fromHandler handler =
  Result $ ReaderT $ \(idt, result) ->
    ExceptT $ handler idt result

-- * Execution

-- TODO: Get rid of by replacing with toHandler
{-# INLINE run #-}
run :: Result a -> Bool -> Pq.Result -> IO (Either ResultError a)
run = toHandler

-- * Construction

{-# INLINE pipelineSync #-}
pipelineSync :: Result ()
pipelineSync = fromResultConsumer ResultConsumer.pipelineSync

{-# INLINE noResult #-}
noResult :: Result ()
noResult = fromResultConsumer ResultConsumer.ok

{-# INLINE rowsAffected #-}
rowsAffected :: Result Int64
rowsAffected = fromResultConsumer ResultConsumer.rowsAffected

{-# INLINE checkExecStatus #-}
checkExecStatus :: [Pq.ExecStatus] -> Result ()
checkExecStatus = fromResultConsumer . ResultConsumer.checkExecStatus

unexpectedResult :: Text -> Result a
unexpectedResult =
  Result . lift . ExceptT . pure . Left . UnexpectedResult

{-# INLINE serverError #-}
serverError :: Result ()
serverError = fromResultConsumer ResultConsumer.serverError

{-# INLINE maybe #-}
maybe :: Row.Row a -> Result (Maybe a)
maybe rowDec =
  do
    checkExecStatus [Pq.TuplesOk]
    Result
      $ ReaderT
      $ \(integerDatetimes, result) -> ExceptT $ do
        maxRows <- Pq.ntuples result
        case maxRows of
          0 -> return (Right Nothing)
          1 -> do
            maxCols <- Pq.nfields result
            let fromRowError (col, err) = RowError 0 col err
            fmap (fmap Just . first fromRowError) $ Row.run rowDec result 0 maxCols integerDatetimes
          _ -> return (Left (UnexpectedAmountOfRows (rowToInt maxRows)))
  where
    rowToInt (Pq.Row n) =
      fromIntegral n

{-# INLINE single #-}
single :: Row.Row a -> Result a
single rowDec =
  do
    checkExecStatus [Pq.TuplesOk]
    Result
      $ ReaderT
      $ \(integerDatetimes, result) -> ExceptT $ do
        maxRows <- Pq.ntuples result
        case maxRows of
          1 -> do
            maxCols <- Pq.nfields result
            let fromRowError (col, err) = RowError 0 col err
            fmap (first fromRowError) $ Row.run rowDec result 0 maxCols integerDatetimes
          _ -> return (Left (UnexpectedAmountOfRows (rowToInt maxRows)))
  where
    rowToInt (Pq.Row n) =
      fromIntegral n

{-# INLINE vector #-}
vector :: Row.Row a -> Result (Vector a)
vector rowDec =
  do
    checkExecStatus [Pq.TuplesOk]
    Result
      $ ReaderT
      $ \(integerDatetimes, result) -> ExceptT $ do
        maxRows <- Pq.ntuples result
        maxCols <- Pq.nfields result
        mvector <- MutableVector.unsafeNew (rowToInt maxRows)
        failureRef <- newIORef Nothing
        forMFromZero_ (rowToInt maxRows) $ \rowIndex -> do
          rowResult <- Row.run rowDec result (intToRow rowIndex) maxCols integerDatetimes
          case rowResult of
            Left !(!colIndex, !x) -> writeIORef failureRef (Just (RowError rowIndex colIndex x))
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
foldl :: (a -> b -> a) -> a -> Row.Row b -> Result a
foldl step init rowDec =
  {-# SCC "foldl" #-}
  do
    checkExecStatus [Pq.TuplesOk]
    Result
      $ ReaderT
      $ \(integerDatetimes, result) ->
        ExceptT
          $ {-# SCC "traversal" #-}
          do
            maxRows <- Pq.ntuples result
            maxCols <- Pq.nfields result
            accRef <- newIORef init
            failureRef <- newIORef Nothing
            forMFromZero_ (rowToInt maxRows) $ \rowIndex -> do
              rowResult <- Row.run rowDec result (intToRow rowIndex) maxCols integerDatetimes
              case rowResult of
                Left !(!colIndex, !x) -> writeIORef failureRef (Just (RowError rowIndex colIndex x))
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
foldr :: (b -> a -> a) -> a -> Row.Row b -> Result a
foldr step init rowDec =
  {-# SCC "foldr" #-}
  do
    checkExecStatus [Pq.TuplesOk]
    Result
      $ ReaderT
      $ \(integerDatetimes, result) -> ExceptT $ do
        maxRows <- Pq.ntuples result
        maxCols <- Pq.nfields result
        accRef <- newIORef init
        failureRef <- newIORef Nothing
        forMToZero_ (rowToInt maxRows) $ \rowIndex -> do
          rowResult <- Row.run rowDec result (intToRow rowIndex) maxCols integerDatetimes
          case rowResult of
            Left !(!colIndex, !x) -> writeIORef failureRef (Just (RowError rowIndex colIndex x))
            Right !x -> modifyIORef accRef (\acc -> step x acc)
        readIORef failureRef >>= \case
          Nothing -> Right <$> readIORef accRef
          Just x -> pure (Left x)
  where
    rowToInt (Pq.Row n) =
      fromIntegral n
    intToRow =
      Pq.Row . fromIntegral
