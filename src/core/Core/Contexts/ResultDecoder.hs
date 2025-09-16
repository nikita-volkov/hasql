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

    -- * Refinement
    refine,

    -- * Relations

    -- ** Handler
    Handler,
    toHandler,
    fromHandler,

    -- ** ResultConsumer
    ResultConsumer,
    toResultConsumer,
    fromResultConsumer,

    -- * Classes
    Wraps (..),
  )
where

import Core.Contexts.ResultConsumer qualified as ResultConsumer
import Core.Contexts.RowDecoder qualified as Row
import Core.Errors
import Data.Vector qualified as Vector
import Data.Vector.Mutable qualified as MutableVector
import Platform.Prelude hiding (foldl, foldr, many, maybe)
import Platform.Prelude qualified as Prelude
import Pq qualified

newtype ResultDecoder a
  = ResultDecoder (ReaderT Pq.Result (ExceptT ResultError IO) a)
  deriving (Functor, Applicative, Monad)

instance Filterable ResultDecoder where
  {-# INLINE mapMaybe #-}
  mapMaybe fn =
    refine (Prelude.maybe (Left "Invalid result") Right . fn)

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

{-# INLINE checkCompatibility #-}
checkCompatibility :: Row.RowDecoder a -> ResultDecoder ()
checkCompatibility rowDec =
  fromHandler \result ->
    Row.toCompatibilityCheck rowDec result

{-# INLINE maybe #-}
maybe :: Row.RowDecoder a -> ResultDecoder (Maybe a)
maybe rowDec =
  do
    checkExecStatus [Pq.TuplesOk]
    checkCompatibility rowDec
    ResultDecoder
      $ ReaderT
      $ \result -> ExceptT $ do
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
single :: Row.RowDecoder a -> ResultDecoder a
single rowDec =
  do
    checkExecStatus [Pq.TuplesOk]
    checkCompatibility rowDec
    ResultDecoder
      $ ReaderT
      $ \result -> ExceptT $ do
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
vector :: Row.RowDecoder a -> ResultDecoder (Vector a)
vector rowDec =
  do
    checkExecStatus [Pq.TuplesOk]
    checkCompatibility rowDec
    ResultDecoder
      $ ReaderT
      $ \result -> ExceptT $ do
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
foldl :: (a -> b -> a) -> a -> Row.RowDecoder b -> ResultDecoder a
foldl step init rowDec =
  {-# SCC "foldl" #-}
  do
    checkExecStatus [Pq.TuplesOk]
    checkCompatibility rowDec
    ResultDecoder
      $ ReaderT
      $ \result ->
        ExceptT
          $ {-# SCC "traversal" #-}
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
foldr :: (b -> a -> a) -> a -> Row.RowDecoder b -> ResultDecoder a
foldr step init rowDec =
  {-# SCC "foldr" #-}
  do
    checkExecStatus [Pq.TuplesOk]
    checkCompatibility rowDec
    ResultDecoder
      $ ReaderT
      $ \result -> ExceptT $ do
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

refine :: (a -> Either Text b) -> ResultDecoder a -> ResultDecoder b
refine refiner (ResultDecoder reader) = ResultDecoder
  $ ReaderT
  $ \result -> ExceptT $ do
    resultEither <- runExceptT $ runReaderT reader result
    return $ resultEither >>= first UnexpectedResult . refiner

-- * Relations

-- ** ResultConsumer

type ResultConsumer a = ResultConsumer.ResultConsumer a

toResultConsumer :: ResultDecoder a -> ResultConsumer a
toResultConsumer (ResultDecoder reader) =
  ResultConsumer.fromHandler \result -> do
    runExceptT (runReaderT reader result)

fromResultConsumer :: ResultConsumer a -> ResultDecoder a
fromResultConsumer resultConsumer =
  fromHandler \result ->
    ResultConsumer.toHandler resultConsumer result

-- ** Handler

type Handler a = Pq.Result -> IO (Either ResultError a)

{-# INLINE toHandler #-}
toHandler :: ResultDecoder a -> Handler a
toHandler (ResultDecoder reader) result =
  runExceptT (runReaderT reader result)

fromHandler :: Handler a -> ResultDecoder a
fromHandler handler =
  ResultDecoder $ ReaderT $ \result ->
    ExceptT $ handler result

-- * Classes

class Wraps f where
  wrap :: ResultDecoder a -> f a
  unwrap :: f a -> ResultDecoder a
