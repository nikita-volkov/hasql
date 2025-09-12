module Core.Contexts.ResultDecoder
  ( ResultDecoder,

    -- * Constructors
    pipelineSync,
    noResult,
    rowsAffected,
    maybe,
    single,
    vector,
    foldl,
    foldr,

    -- * Relations

    -- ** Handler
    Handler,
    toHandler,
    fromHandler,

    -- ** ResultConsumerByIdt
    ResultConsumerByIdt,
    toResultConsumerByIdt,
    fromResultConsumerByIdt,
  )
where

import Core.Contexts.ResultConsumer qualified as ResultConsumer
import Core.Contexts.RowDecoder qualified as Row
import Core.Errors
import Data.Vector qualified as Vector
import Data.Vector.Mutable qualified as MutableVector
import Platform.Prelude hiding (foldl, foldr, many, maybe)
import Pq qualified

newtype ResultDecoder a
  = ResultDecoder (ReaderT (Bool, Pq.Result) (ExceptT ResultError IO) a)
  deriving (Functor, Applicative, Monad)

-- * Construction

{-# INLINE pipelineSync #-}
pipelineSync :: ResultDecoder ()
pipelineSync = fromResultConsumer ResultConsumer.pipelineSync

{-# INLINE noResult #-}
noResult :: ResultDecoder ()
noResult = fromResultConsumer ResultConsumer.ok

{-# INLINE rowsAffected #-}
rowsAffected :: ResultDecoder Int64
rowsAffected = fromResultConsumer ResultConsumer.rowsAffected

{-# INLINE checkExecStatus #-}
checkExecStatus :: [Pq.ExecStatus] -> ResultDecoder ()
checkExecStatus = fromResultConsumer . ResultConsumer.checkExecStatus

{-# INLINE maybe #-}
maybe :: Row.RowDecoder a -> ResultDecoder (Maybe a)
maybe rowDec =
  do
    checkExecStatus [Pq.TuplesOk]
    ResultDecoder
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
single :: Row.RowDecoder a -> ResultDecoder a
single rowDec =
  do
    checkExecStatus [Pq.TuplesOk]
    ResultDecoder
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
vector :: Row.RowDecoder a -> ResultDecoder (Vector a)
vector rowDec =
  do
    checkExecStatus [Pq.TuplesOk]
    ResultDecoder
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
foldl :: (a -> b -> a) -> a -> Row.RowDecoder b -> ResultDecoder a
foldl step init rowDec =
  {-# SCC "foldl" #-}
  do
    checkExecStatus [Pq.TuplesOk]
    ResultDecoder
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
foldr :: (b -> a -> a) -> a -> Row.RowDecoder b -> ResultDecoder a
foldr step init rowDec =
  {-# SCC "foldr" #-}
  do
    checkExecStatus [Pq.TuplesOk]
    ResultDecoder
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

-- * Relations

-- ** ResultConsumerByIdt

type ResultConsumerByIdt a = Bool -> ResultConsumer.ResultConsumer a

toResultConsumerByIdt :: ResultDecoder a -> ResultConsumerByIdt a
toResultConsumerByIdt (ResultDecoder reader) idt =
  ResultConsumer.fromHandler \result -> do
    runExceptT (runReaderT reader (idt, result))

fromResultConsumerByIdt :: ResultConsumerByIdt a -> ResultDecoder a
fromResultConsumerByIdt resultConsumerByIdt =
  fromHandler \idt result ->
    ResultConsumer.toHandler (resultConsumerByIdt idt) result

-- ** ResultConsumer

type ResultConsumer a = ResultConsumer.ResultConsumer a

fromResultConsumer :: ResultConsumer a -> ResultDecoder a
fromResultConsumer handler =
  fromHandler \_idt result ->
    ResultConsumer.toHandler handler result

-- ** Handler

type Handler a = Bool -> Pq.Result -> IO (Either ResultError a)

{-# INLINE toHandler #-}
toHandler :: ResultDecoder a -> Handler a
toHandler (ResultDecoder reader) idt result =
  runExceptT (runReaderT reader (idt, result))

fromHandler :: Handler a -> ResultDecoder a
fromHandler handler =
  ResultDecoder $ ReaderT $ \(idt, result) ->
    ExceptT $ handler idt result
