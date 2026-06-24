module Hasql.Comms.ResultDecoder
  ( ResultDecoder,

    -- * Relations
    Handler,
    toHandler,
    fromHandler,

    -- * Extractors
    columnOids,

    -- * Constructors

    -- ** Basic
    ok,
    pipelineSync,
    rowsAffected,
    checkExecStatus,

    -- ** Higher-level decoders
    maybe,
    single,
    vector,
    foldl,
    foldr,

    -- ** Refinement
    refine,

    -- * Errors
    Error (..),
  )
where

import Data.Attoparsec.ByteString.Char8 qualified as Attoparsec
import Data.ByteString qualified as ByteString
import Data.Vector qualified as Vector
import Data.Vector.Mutable qualified as MutableVector
import Hasql.Comms.RowDecoder qualified as RowDecoder
import Hasql.Comms.RowReader qualified as RowReader
import Hasql.Platform.Prelude hiding (foldl, foldr, maybe)
import Hasql.Platform.Prelude qualified as Prelude
import Pqi qualified

-- | Result consumption context, for consuming a single result from a sequence of results returned by the server.
newtype ResultDecoder a
  = ResultDecoder (forall r. (Pqi.IsResult r) => r -> IO (Either Error a))

instance Functor ResultDecoder where
  fmap f (ResultDecoder g) = ResultDecoder $ \r -> fmap (fmap f) (g r)

instance Applicative ResultDecoder where
  pure a = ResultDecoder $ \_ -> pure (Right a)
  ResultDecoder f <*> ResultDecoder x = ResultDecoder $ \r -> (<*>) <$> f r <*> x r

instance Monad ResultDecoder where
  ResultDecoder x >>= f = ResultDecoder $ \r ->
    x r >>= either (pure . Left) (\a -> let ResultDecoder g = f a in g r)

instance MonadError Error ResultDecoder where
  throwError e = ResultDecoder $ \_ -> pure (Left e)
  catchError (ResultDecoder x) handler = ResultDecoder $ \r ->
    x r >>= either (\e -> let ResultDecoder g = handler e in g r) (pure . Right)

instance Filterable ResultDecoder where
  {-# INLINE mapMaybe #-}
  mapMaybe fn =
    refine (Prelude.maybe (Left "Invalid result") Right . fn)

-- * Relations

-- ** Handler

type Handler a = forall r. (Pqi.IsResult r) => r -> IO (Either Error a)

toHandler :: ResultDecoder a -> Handler a
toHandler (ResultDecoder handler) =
  handler

fromHandler :: Handler a -> ResultDecoder a
fromHandler handler =
  ResultDecoder handler

-- * Construction

{-# INLINE ok #-}
ok :: ResultDecoder ()
ok = checkExecStatus [Pqi.CommandOk, Pqi.TuplesOk]

{-# INLINE pipelineSync #-}
pipelineSync :: ResultDecoder ()
pipelineSync = checkExecStatus [Pqi.PipelineSync]

{-# INLINE rowsAffected #-}
rowsAffected :: ResultDecoder Int64
rowsAffected = do
  checkExecStatus [Pqi.CommandOk]
  ResultDecoder \r -> do
    cmdTuplesReader <$> Pqi.cmdTuples r
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
checkExecStatus :: [Pqi.ExecStatus] -> ResultDecoder ()
checkExecStatus expectedList = do
  status <- ResultDecoder \r -> Right <$> Pqi.resultStatus r
  unless (elem status expectedList) $ do
    case status of
      Pqi.BadResponse -> serverError
      Pqi.NonfatalError -> serverError
      Pqi.FatalError -> serverError
      Pqi.EmptyQuery -> return ()
      _ ->
        throwError
          ( UnexpectedResult
              ("Unexpected result status: " <> fromString (show status) <> ". Expecting one of the following: " <> fromString (show expectedList))
          )

{-# INLINE serverError #-}
serverError :: ResultDecoder ()
serverError =
  ResultDecoder \r -> do
    code <-
      fold <$> Pqi.resultErrorField r Pqi.DiagSqlstate
    message <-
      fold <$> Pqi.resultErrorField r Pqi.DiagMessagePrimary
    detail <-
      Pqi.resultErrorField r Pqi.DiagMessageDetail
    hint <-
      Pqi.resultErrorField r Pqi.DiagMessageHint
    position <-
      parsePosition <$> Pqi.resultErrorField r Pqi.DiagStatementPosition
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
columnOids :: ResultDecoder [Word32]
columnOids = ResultDecoder \r -> do
  count <- fromIntegral <$> Pqi.nfields r
  oids <- forM [0 .. count - 1] $ \colIndex ->
    Pqi.ftype r colIndex
  pure (Right oids)

-- * Higher-level decoders

{-# INLINE checkCompatibility #-}
checkCompatibility :: RowDecoder.RowDecoder a -> ResultDecoder ()
checkCompatibility rowDec =
  let oids = RowDecoder.toExpectedOids rowDec
   in ResultDecoder \r -> do
        maxCols <- fromIntegral <$> Pqi.nfields r
        if length oids == maxCols
          then
            let go [] _ = pure (Right ())
                go (Nothing : rest) colIndex = go rest (succ colIndex)
                go (Just expectedOid : rest) colIndex = do
                  actualOid <- Pqi.ftype r colIndex
                  if actualOid == expectedOid
                    then go rest (succ colIndex)
                    else
                      pure
                        ( Left
                            ( DecoderTypeMismatch
                                (fromIntegral colIndex)
                                expectedOid
                                actualOid
                            )
                        )
             in go oids 0
          else pure (Left (UnexpectedColumnCount (length oids) maxCols))

{-# INLINE maybe #-}
maybe :: RowDecoder.RowDecoder a -> ResultDecoder (Maybe a)
maybe rowDec =
  do
    checkExecStatus [Pqi.TuplesOk]
    checkCompatibility rowDec
    ResultDecoder
      $ \r -> do
        maxRows <- Pqi.ntuples r
        case maxRows of
          0 -> return (Right Nothing)
          1 -> do
            decResult <-
              RowReader.toHandler (RowDecoder.toRowReader rowDec) r 0
                <&> first (RowError 0)
            pure (fmap Just decResult)
          _ -> return (Left (UnexpectedRowCount (fromIntegral maxRows)))

{-# INLINE single #-}
single :: RowDecoder.RowDecoder a -> ResultDecoder a
single rowDec =
  do
    checkExecStatus [Pqi.TuplesOk]
    checkCompatibility rowDec
    ResultDecoder
      $ \r -> do
        maxRows <- Pqi.ntuples r
        case maxRows of
          1 -> do
            RowReader.toHandler (RowDecoder.toRowReader rowDec) r 0
              <&> first (RowError 0)
          _ -> return (Left (UnexpectedRowCount (fromIntegral maxRows)))

{-# INLINE vector #-}
vector :: RowDecoder.RowDecoder a -> ResultDecoder (Vector a)
vector rowDec =
  do
    checkExecStatus [Pqi.TuplesOk]
    checkCompatibility rowDec
    ResultDecoder
      $ \r -> do
        maxRows <- fromIntegral <$> Pqi.ntuples r
        mvector <- MutableVector.unsafeNew maxRows
        failureRef <- newIORef Nothing
        forMFromZero_ maxRows $ \rowIndex -> do
          rowResult <- RowReader.toHandler (RowDecoder.toRowReader rowDec) r (fromIntegral rowIndex)
          case rowResult of
            Left !err -> writeIORef failureRef (Just (RowError rowIndex err))
            Right !x -> MutableVector.unsafeWrite mvector rowIndex x
        readIORef failureRef >>= \case
          Nothing -> Right <$> Vector.unsafeFreeze mvector
          Just x -> pure (Left x)

{-# INLINE foldl #-}
foldl :: (a -> b -> a) -> a -> RowDecoder.RowDecoder b -> ResultDecoder a
foldl step init rowDec =
  {-# SCC "foldl" #-}
  do
    checkExecStatus [Pqi.TuplesOk]
    checkCompatibility rowDec
    ResultDecoder
      $ \r ->
        {-# SCC "traversal" #-}
        do
          maxRows <- fromIntegral <$> Pqi.ntuples r
          accRef <- newIORef init
          failureRef <- newIORef Nothing
          forMFromZero_ maxRows $ \rowIndex -> do
            rowResult <- RowReader.toHandler (RowDecoder.toRowReader rowDec) r (fromIntegral rowIndex)
            case rowResult of
              Left !err -> writeIORef failureRef (Just (RowError rowIndex err))
              Right !x -> modifyIORef' accRef (\acc -> step acc x)
          readIORef failureRef >>= \case
            Nothing -> Right <$> readIORef accRef
            Just x -> pure (Left x)

{-# INLINE foldr #-}
foldr :: (b -> a -> a) -> a -> RowDecoder.RowDecoder b -> ResultDecoder a
foldr step init rowDec =
  {-# SCC "foldr" #-}
  do
    checkExecStatus [Pqi.TuplesOk]
    checkCompatibility rowDec
    ResultDecoder
      $ \r -> do
        maxRows <- fromIntegral <$> Pqi.ntuples r
        accRef <- newIORef init
        failureRef <- newIORef Nothing
        forMToZero_ maxRows $ \rowIndex -> do
          rowResult <- RowReader.toHandler (RowDecoder.toRowReader rowDec) r (fromIntegral rowIndex)
          case rowResult of
            Left !err -> writeIORef failureRef (Just (RowError rowIndex err))
            Right !x -> modifyIORef accRef (\acc -> step x acc)
        readIORef failureRef >>= \case
          Nothing -> Right <$> readIORef accRef
          Just x -> pure (Left x)

-- * Refinement

refine :: (a -> Either Text b) -> ResultDecoder a -> ResultDecoder b
refine refiner (ResultDecoder reader) = ResultDecoder
  $ \r -> do
    resultEither <- reader r
    return $ resultEither >>= first UnexpectedResult . refiner

-- * Errors

-- |
-- An error with a command result.
data Error
  = -- | An error reported by the DB.
    ServerError
      -- | __Code__. The SQLSTATE code for the error. It's recommended to use
      -- <http://hackage.haskell.org/package/postgresql-error-codes
      -- the "postgresql-error-codes" package> to work with those.
      ByteString
      -- | __Message__. The primary human-readable error message(typically one
      -- line). Always present.
      ByteString
      -- | __Details__. An optional secondary error message carrying more
      -- detail about the problem. Might run to multiple lines.
      (Maybe ByteString)
      -- | __Hint__. An optional suggestion on what to do about the problem.
      -- This is intended to differ from detail in that it offers advice
      -- (potentially inappropriate) rather than hard facts. Might run to
      -- multiple lines.
      (Maybe ByteString)
      -- | __Position__. Error cursor position as an index into the original
      -- statement string. Positions are measured in characters not bytes.
      (Maybe Int)
  | -- |
    -- The database returned an unexpected result.
    -- Indicates an improper statement or a schema mismatch.
    UnexpectedResult Text
  | -- |
    -- An unexpected amount of rows.
    UnexpectedRowCount Int
  | -- |
    -- An unexpected amount of columns in the result.
    UnexpectedColumnCount
      -- | Expected amount of columns.
      Int
      -- | Actual amount of columns.
      Int
  | -- |
    -- Appears when the decoder's expected type doesn't match the actual column type.
    -- Reports the expected OID and the actual OID from the result.
    DecoderTypeMismatch
      -- | Column index.
      Int
      -- | Expected OID.
      Word32
      -- | Actual OID.
      Word32
  | -- | An error in a specific row, reported by a row decoder.
    RowError
      -- | Row index.
      Int
      -- | Underlying error.
      RowDecoder.Error
  deriving (Show, Eq)
