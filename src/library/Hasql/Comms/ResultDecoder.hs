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

-- | Defunctionalized plan for consuming a single result from the server.
--
-- Each higher-level decoder ('maybe', 'single', 'vector', 'foldl', 'foldr')
-- is its own constructor rather than a value built out of smaller monadic
-- actions, so 'runPlan' can interpret it with one direct-'IO' case analysis
-- per result instead of running generic 'Applicative'\/'Monad' machinery at
-- every step.
data Plan a where
  CheckExecStatus :: [Pq.ExecStatus] -> Plan ()
  RowsAffected :: Plan Int64
  ColumnOids :: Plan [Pq.Oid]
  MaybeRow :: RowDecoder.RowDecoder a -> Plan (Prelude.Maybe a)
  SingleRow :: RowDecoder.RowDecoder a -> Plan a
  VectorRows :: RowDecoder.RowDecoder a -> Plan (Vector a)
  FoldlRows :: (a -> b -> a) -> a -> RowDecoder.RowDecoder b -> Plan a
  FoldrRows :: (b -> a -> a) -> a -> RowDecoder.RowDecoder b -> Plan a
  Refine :: (a -> Either Text b) -> Plan a -> Plan b
  Custom :: (Pq.Result -> IO (Either Error a)) -> Plan a

-- | Result consumption context, for consuming a single result from a sequence of results returned by the server.
newtype ResultDecoder a
  = ResultDecoder (Plan a)

instance Functor ResultDecoder where
  {-# INLINE fmap #-}
  fmap f (ResultDecoder plan) = ResultDecoder (Refine (Right . f) plan)

instance Filterable ResultDecoder where
  {-# INLINE mapMaybe #-}
  mapMaybe fn =
    refine (Prelude.maybe (Left "Invalid result") Right . fn)

-- * Relations

-- ** Handler

type Handler a = Pq.Result -> IO (Either Error a)

toHandler :: ResultDecoder a -> Handler a
toHandler (ResultDecoder plan) = runPlan plan

fromHandler :: Handler a -> ResultDecoder a
fromHandler handler =
  ResultDecoder (Custom handler)

-- * Construction

{-# INLINE ok #-}
ok :: ResultDecoder ()
ok = checkExecStatus [Pq.CommandOk, Pq.TuplesOk]

{-# INLINE pipelineSync #-}
pipelineSync :: ResultDecoder ()
pipelineSync = checkExecStatus [Pq.PipelineSync]

{-# INLINE checkExecStatus #-}
checkExecStatus :: [Pq.ExecStatus] -> ResultDecoder ()
checkExecStatus expected = ResultDecoder (CheckExecStatus expected)

{-# INLINE rowsAffected #-}
rowsAffected :: ResultDecoder Int64
rowsAffected = ResultDecoder RowsAffected

-- | Get the OIDs of all columns in the current result.
{-# INLINE columnOids #-}
columnOids :: ResultDecoder [Pq.Oid]
columnOids = ResultDecoder ColumnOids

-- * Higher-level decoders

{-# INLINE maybe #-}
maybe :: RowDecoder.RowDecoder a -> ResultDecoder (Prelude.Maybe a)
maybe rowDec = ResultDecoder (MaybeRow rowDec)

{-# INLINE single #-}
single :: RowDecoder.RowDecoder a -> ResultDecoder a
single rowDec = ResultDecoder (SingleRow rowDec)

{-# INLINE vector #-}
vector :: RowDecoder.RowDecoder a -> ResultDecoder (Vector a)
vector rowDec = ResultDecoder (VectorRows rowDec)

{-# INLINE foldl #-}
foldl :: (a -> b -> a) -> a -> RowDecoder.RowDecoder b -> ResultDecoder a
foldl step init0 rowDec = ResultDecoder (FoldlRows step init0 rowDec)

{-# INLINE foldr #-}
foldr :: (b -> a -> a) -> a -> RowDecoder.RowDecoder b -> ResultDecoder a
foldr step init0 rowDec = ResultDecoder (FoldrRows step init0 rowDec)

-- * Refinement

refine :: (a -> Either Text b) -> ResultDecoder a -> ResultDecoder b
refine refiner (ResultDecoder plan) = ResultDecoder (Refine refiner plan)

-- * Interpreter

--
-- 'runPlan' and its helpers are all 'INLINABLE' so GHC can specialize them
-- at each call site in 'Hasql.Comms.Recv', eliminating the dictionary
-- dispatch that the old 'ReaderT'\/'ExceptT'-derived 'ResultDecoder' paid
-- for on every accessor call.

{-# INLINEABLE runPlan #-}
runPlan :: Plan a -> Pq.Result -> IO (Either Error a)
runPlan plan result = case plan of
  CheckExecStatus expected -> checkStatus expected result
  RowsAffected -> do
    statusResult <- checkStatus [Pq.CommandOk] result
    case statusResult of
      Left err -> pure (Left err)
      Right () -> readAffectedRows result
  ColumnOids -> Right <$> readColumnOids result
  MaybeRow rowDec -> withRows [Pq.TuplesOk] rowDec result \maxRows ->
    case maxRows of
      0 -> pure (Right Prelude.Nothing)
      1 -> fmap (fmap Prelude.Just) (decodeRow rowDec result (intToRow 0))
      _ -> pure (Left (UnexpectedRowCount maxRows))
  SingleRow rowDec -> withRows [Pq.TuplesOk] rowDec result \maxRows ->
    case maxRows of
      1 -> decodeRow rowDec result (intToRow 0)
      _ -> pure (Left (UnexpectedRowCount maxRows))
  VectorRows rowDec -> withRows [Pq.TuplesOk] rowDec result \maxRows -> do
    mvector <- MutableVector.unsafeNew maxRows
    failureRef <- newIORef Prelude.Nothing
    forMFromZero_ maxRows \rowIndex -> do
      rowResult <- decodeRow rowDec result (intToRow rowIndex)
      case rowResult of
        Left !err -> writeIORef failureRef (Prelude.Just err)
        Right !x -> MutableVector.unsafeWrite mvector rowIndex x
    readIORef failureRef >>= \case
      Prelude.Nothing -> Right <$> Vector.unsafeFreeze mvector
      Prelude.Just err -> pure (Left err)
  FoldlRows step init0 rowDec -> withRows [Pq.TuplesOk] rowDec result \maxRows -> do
    accRef <- newIORef init0
    failureRef <- newIORef Prelude.Nothing
    forMFromZero_ maxRows \rowIndex -> do
      rowResult <- decodeRow rowDec result (intToRow rowIndex)
      case rowResult of
        Left !err -> writeIORef failureRef (Prelude.Just err)
        Right !x -> modifyIORef' accRef (\acc -> step acc x)
    readIORef failureRef >>= \case
      Prelude.Nothing -> Right <$> readIORef accRef
      Prelude.Just err -> pure (Left err)
  FoldrRows step init0 rowDec -> withRows [Pq.TuplesOk] rowDec result \maxRows -> do
    accRef <- newIORef init0
    failureRef <- newIORef Prelude.Nothing
    forMToZero_ maxRows \rowIndex -> do
      rowResult <- decodeRow rowDec result (intToRow rowIndex)
      case rowResult of
        Left !err -> writeIORef failureRef (Prelude.Just err)
        Right !x -> modifyIORef accRef (\acc -> step x acc)
    readIORef failureRef >>= \case
      Prelude.Nothing -> Right <$> readIORef accRef
      Prelude.Just err -> pure (Left err)
  Refine refiner inner -> do
    innerResult <- runPlan inner result
    pure case innerResult of
      Left err -> Left err
      Right a -> first UnexpectedResult (refiner a)
  Custom handler -> handler result

-- | Check exec status and OID compatibility, then hand the row count (as an
-- 'Int') to the continuation. Shared by every row-consuming plan variant.
{-# INLINEABLE withRows #-}
withRows ::
  [Pq.ExecStatus] ->
  RowDecoder.RowDecoder a ->
  Pq.Result ->
  (Int -> IO (Either Error b)) ->
  IO (Either Error b)
withRows expectedStatus rowDec result k = do
  statusResult <- checkStatus expectedStatus result
  case statusResult of
    Left err -> pure (Left err)
    Right () -> do
      compatResult <- checkCompatibility rowDec result
      case compatResult of
        Left err -> pure (Left err)
        Right () -> do
          maxRows <- rowToInt <$> Pq.ntuples result
          k maxRows

{-# INLINEABLE checkStatus #-}
checkStatus :: [Pq.ExecStatus] -> Pq.Result -> IO (Either Error ())
checkStatus expectedList result = do
  status <- Pq.resultStatus result
  if elem status expectedList
    then pure (Right ())
    else case status of
      Pq.BadResponse -> Left <$> readServerError result
      Pq.NonfatalError -> Left <$> readServerError result
      Pq.FatalError -> Left <$> readServerError result
      Pq.EmptyQuery -> pure (Right ())
      _ ->
        pure
          ( Left
              ( UnexpectedResult
                  ("Unexpected result status: " <> fromString (show status) <> ". Expecting one of the following: " <> fromString (show expectedList))
              )
          )

{-# INLINEABLE readServerError #-}
readServerError :: Pq.Result -> IO Error
readServerError result = do
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
  pure (ServerError code message detail hint position)
  where
    parsePosition = \case
      Prelude.Nothing -> Prelude.Nothing
      Prelude.Just pos ->
        case Attoparsec.parseOnly (Attoparsec.decimal <* Attoparsec.endOfInput) pos of
          Right pos' -> Prelude.Just pos'
          _ -> Prelude.Nothing

{-# INLINEABLE readAffectedRows #-}
readAffectedRows :: Pq.Result -> IO (Either Error Int64)
readAffectedRows result =
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

{-# INLINEABLE readColumnOids #-}
readColumnOids :: Pq.Result -> IO [Pq.Oid]
readColumnOids result = do
  columnsAmount <- Pq.nfields result
  let Pq.Col count = columnsAmount
  forM [0 .. count - 1] \colIndex ->
    Pq.ftype result (Pq.Col colIndex)

{-# INLINEABLE checkCompatibility #-}
checkCompatibility :: RowDecoder.RowDecoder a -> Pq.Result -> IO (Either Error ())
checkCompatibility rowDec result =
  let oids = RowDecoder.toExpectedOids rowDec
   in do
        maxCols <- Pq.nfields result
        if length oids == Pq.colToInt maxCols
          then
            let go [] _ = pure (Right ())
                go (Prelude.Nothing : rest) colIndex = go rest (succ colIndex)
                go (Prelude.Just expectedOid : rest) colIndex = do
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

{-# INLINEABLE decodeRow #-}
decodeRow :: RowDecoder.RowDecoder a -> Pq.Result -> Pq.Row -> IO (Either Error a)
decodeRow rowDec result row =
  RowDecoder.toDecoder rowDec result row
    <&> first (RowError (rowToInt row))

{-# INLINE rowToInt #-}
rowToInt :: Pq.Row -> Int
rowToInt (Pq.Row n) = fromIntegral n

{-# INLINE intToRow #-}
intToRow :: Int -> Pq.Row
intToRow = Pq.Row . fromIntegral

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
