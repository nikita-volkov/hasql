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
import Hasql.Platform.Prelude hiding (foldl, foldr, maybe)
import Hasql.Platform.Prelude qualified as Prelude
import Hasql.Pq qualified as Pq

-- | Result consumption context, for consuming a single result from a sequence of results returned by the server.
newtype ResultDecoder a
  = ResultDecoder (Pq.Result -> IO (Either Error a))
  deriving
    (Functor, Applicative, Monad, MonadError Error, MonadReader Pq.Result)
    via (ReaderT Pq.Result (ExceptT Error IO))

instance Filterable ResultDecoder where
  {-# INLINE mapMaybe #-}
  mapMaybe fn =
    refine (Prelude.maybe (Left "Invalid result") Right . fn)

-- * Relations

-- ** Handler

type Handler a = Pq.Result -> IO (Either Error a)

toHandler :: ResultDecoder a -> Handler a
toHandler (ResultDecoder handler) =
  handler

fromHandler :: Handler a -> ResultDecoder a
fromHandler handler =
  ResultDecoder handler

-- * Construction

{-# INLINE ok #-}
ok :: ResultDecoder ()
ok = checkExecStatus [Pq.CommandOk, Pq.TuplesOk]

{-# INLINE pipelineSync #-}
pipelineSync :: ResultDecoder ()
pipelineSync = checkExecStatus [Pq.PipelineSync]

{-# INLINE rowsAffected #-}
rowsAffected :: ResultDecoder Int64
rowsAffected = do
  checkExecStatus [Pq.CommandOk]
  ResultDecoder \result -> do
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
checkExecStatus :: [Pq.ExecStatus] -> ResultDecoder ()
checkExecStatus expectedList = do
  status <- ResultDecoder \result -> Right <$> Pq.resultStatus result
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
serverError :: ResultDecoder ()
serverError =
  ResultDecoder \result -> do
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
columnOids :: ResultDecoder [Pq.Oid]
columnOids = ResultDecoder \result -> do
  columnsAmount <- Pq.nfields result
  let Pq.Col count = columnsAmount
  oids <- forM [0 .. count - 1] $ \colIndex ->
    Pq.ftype result (Pq.Col colIndex)
  pure (Right oids)

-- * Higher-level decoders

{-# INLINE checkCompatibility #-}
checkCompatibility :: RowDecoder.RowDecoder a -> ResultDecoder ()
checkCompatibility rowDec =
  let oids = RowDecoder.toExpectedOids rowDec
   in ResultDecoder \result -> do
        maxCols <- Pq.nfields result
        if length oids == Pq.colToInt maxCols
          then
            let go [] _ = pure (Right ())
                go (Nothing : rest) colIndex = go rest (succ colIndex)
                go (Just expectedOid : rest) colIndex = do
                  actualOid <- Pq.ftype result (Pq.toColumn colIndex)
                  if actualOid == expectedOid
                    then go rest (succ colIndex)
                    else
                      pure
                        ( Left
                            ( DecoderTypeMismatch
                                colIndex
                                (Pq.oidToWord32 expectedOid)
                                (Pq.oidToWord32 actualOid)
                            )
                        )
             in go oids 0
          else pure (Left (UnexpectedColumnCount (length oids) (Pq.colToInt maxCols)))

{-# INLINE maybe #-}
maybe :: RowDecoder.RowDecoder a -> ResultDecoder (Maybe a)
maybe rowDec =
  do
    checkExecStatus [Pq.TuplesOk]
    checkCompatibility rowDec
    ResultDecoder
      $ \result -> do
        maxRows <- Pq.ntuples result
        case maxRows of
          0 -> return (Right Nothing)
          1 -> do
            result <-
              RowDecoder.toDecoder rowDec result 0
                <&> first (RowError 0)
            pure (fmap Just result)
          _ -> return (Left (UnexpectedRowCount (rowToInt maxRows)))
  where
    rowToInt (Pq.Row n) =
      fromIntegral n

{-# INLINE single #-}
single :: RowDecoder.RowDecoder a -> ResultDecoder a
single rowDec =
  do
    checkExecStatus [Pq.TuplesOk]
    checkCompatibility rowDec
    ResultDecoder
      $ \result -> do
        maxRows <- Pq.ntuples result
        case maxRows of
          1 -> do
            RowDecoder.toDecoder rowDec result 0
              <&> first (RowError 0)
          _ -> return (Left (UnexpectedRowCount (rowToInt maxRows)))
  where
    rowToInt (Pq.Row n) =
      fromIntegral n

{-# INLINE vector #-}
vector :: RowDecoder.RowDecoder a -> ResultDecoder (Vector a)
vector rowDec =
  do
    checkExecStatus [Pq.TuplesOk]
    checkCompatibility rowDec
    ResultDecoder
      $ \result -> do
        maxRows <- Pq.ntuples result
        mvector <- MutableVector.unsafeNew (rowToInt maxRows)
        failureRef <- newIORef Nothing
        forMFromZero_ (rowToInt maxRows) $ \rowIndex -> do
          rowResult <- RowDecoder.toDecoder rowDec result (intToRow rowIndex)
          case rowResult of
            Left !err -> writeIORef failureRef (Just (RowError rowIndex err))
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
foldl :: (a -> b -> a) -> a -> RowDecoder.RowDecoder b -> ResultDecoder a
foldl step init rowDec =
  {-# SCC "foldl" #-}
  do
    checkExecStatus [Pq.TuplesOk]
    checkCompatibility rowDec
    ResultDecoder
      $ \result ->
        {-# SCC "traversal" #-}
        do
          maxRows <- Pq.ntuples result
          accRef <- newIORef init
          failureRef <- newIORef Nothing
          forMFromZero_ (rowToInt maxRows) $ \rowIndex -> do
            rowResult <- RowDecoder.toDecoder rowDec result (intToRow rowIndex)
            case rowResult of
              Left !err -> writeIORef failureRef (Just (RowError rowIndex err))
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
foldr :: (b -> a -> a) -> a -> RowDecoder.RowDecoder b -> ResultDecoder a
foldr step init rowDec =
  {-# SCC "foldr" #-}
  do
    checkExecStatus [Pq.TuplesOk]
    checkCompatibility rowDec
    ResultDecoder
      $ \result -> do
        maxRows <- Pq.ntuples result
        accRef <- newIORef init
        failureRef <- newIORef Nothing
        forMToZero_ (rowToInt maxRows) $ \rowIndex -> do
          rowResult <- RowDecoder.toDecoder rowDec result (intToRow rowIndex)
          case rowResult of
            Left !err -> writeIORef failureRef (Just (RowError rowIndex err))
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
  $ \result -> do
    resultEither <- reader result
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
