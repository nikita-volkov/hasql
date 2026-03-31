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
import Data.HashMap.Strict qualified as HashMap
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
checkCompatibility :: HashMap Word32 Word32 -> RowDecoder.RowDecoder a -> ResultDecoder ()
checkCompatibility domainMap rowDec =
  let oids = RowDecoder.toExpectedOids rowDec
   in ResultDecoder \result -> do
        maxCols <- Pq.nfields result
        if length oids == Pq.colToInt maxCols
          then
            let go [] _ = pure (Right ())
                go (Nothing : rest) colIndex = go rest (succ colIndex)
                go (Just expectedOid : rest) colIndex = do
                  actualOid <- Pq.ftype result (Pq.toColumn colIndex)
                  if actualOid == expectedOid || isDomainOf domainMap (Pq.oidToWord32 actualOid) (Pq.oidToWord32 expectedOid)
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
maybe :: HashMap Word32 Word32 -> RowDecoder.RowDecoder a -> ResultDecoder (Maybe a)
maybe domainMap rowDec =
  do
    checkExecStatus [Pq.TuplesOk]
    checkCompatibility domainMap rowDec
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
single :: HashMap Word32 Word32 -> RowDecoder.RowDecoder a -> ResultDecoder a
single domainMap rowDec =
  do
    checkExecStatus [Pq.TuplesOk]
    checkCompatibility domainMap rowDec
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
vector :: HashMap Word32 Word32 -> RowDecoder.RowDecoder a -> ResultDecoder (Vector a)
vector domainMap rowDec =
  do
    checkExecStatus [Pq.TuplesOk]
    checkCompatibility domainMap rowDec
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
foldl :: HashMap Word32 Word32 -> (a -> b -> a) -> a -> RowDecoder.RowDecoder b -> ResultDecoder a
foldl domainMap step init rowDec =
  {-# SCC "foldl" #-}
  do
    checkExecStatus [Pq.TuplesOk]
    checkCompatibility domainMap rowDec
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
foldr :: HashMap Word32 Word32 -> (b -> a -> a) -> a -> RowDecoder.RowDecoder b -> ResultDecoder a
foldr domainMap step init rowDec =
  {-# SCC "foldr" #-}
  do
    checkExecStatus [Pq.TuplesOk]
    checkCompatibility domainMap rowDec
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

-- * Domain type resolution

-- | Check if the actual OID is a domain type whose ultimate base type matches the expected OID.
{-# INLINE isDomainOf #-}
isDomainOf :: HashMap Word32 Word32 -> Word32 -> Word32 -> Bool
isDomainOf domainMap actualOid expectedOid =
  case resolveBaseType domainMap actualOid of
    Nothing -> False
    Just baseOid -> baseOid == expectedOid

-- | Resolve a potentially domain OID to its ultimate base type OID.
-- Returns 'Nothing' if the OID is not a domain type.
-- Follows chains of nested domains (domain of domain).
{-# INLINE resolveBaseType #-}
resolveBaseType :: HashMap Word32 Word32 -> Word32 -> Maybe Word32
resolveBaseType domainMap oid =
  case HashMap.lookup oid domainMap of
    Nothing -> Nothing
    Just baseOid -> Just (followChain baseOid)
  where
    followChain currentOid =
      case HashMap.lookup currentOid domainMap of
        Nothing -> currentOid
        Just nextOid -> followChain nextOid
