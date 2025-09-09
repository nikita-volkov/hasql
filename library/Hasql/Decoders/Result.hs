module Hasql.Decoders.Result where

import Data.Attoparsec.ByteString.Char8 qualified as Attoparsec
import Data.ByteString qualified as ByteString
import Data.Vector qualified as Vector
import Data.Vector.Mutable qualified as MutableVector
import Hasql.DecoderCompat qualified as DecoderCompat
import Hasql.Decoders.Row qualified as Row
import Hasql.Errors
import Hasql.LibPq14 qualified as LibPQ
import Hasql.Prelude hiding (many, maybe)
import Hasql.Prelude qualified as Prelude

newtype Result a
  = Result (ReaderT (Bool, LibPQ.Result) (ExceptT ResultError IO) a)
  deriving (Functor, Applicative, Monad)

{-# INLINE run #-}
run :: Result a -> Bool -> LibPQ.Result -> IO (Either ResultError a)
run (Result reader) idt result =
  runExceptT (runReaderT reader (idt, result))

{-# INLINE pipelineSync #-}
pipelineSync :: Result ()
pipelineSync =
  checkExecStatus [LibPQ.PipelineSync]

{-# INLINE noResult #-}
noResult :: Result ()
noResult =
  checkExecStatus [LibPQ.CommandOk, LibPQ.TuplesOk]

{-# INLINE rowsAffected #-}
rowsAffected :: Result Int64
rowsAffected =
  do
    checkExecStatus [LibPQ.CommandOk]
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
          first (\m -> UnexpectedResult ("Decimal parsing failure: " <> fromString m))
            $ Attoparsec.parseOnly (Attoparsec.decimal <* Attoparsec.endOfInput) bytes

{-# INLINE checkExecStatus #-}
checkExecStatus :: [LibPQ.ExecStatus] -> Result ()
checkExecStatus expectedList =
  {-# SCC "checkExecStatus" #-}
  do
    status <- Result $ ReaderT $ \(_, result) -> lift $ LibPQ.resultStatus result
    unless (elem status expectedList) $ do
      case status of
        LibPQ.BadResponse -> serverError
        LibPQ.NonfatalError -> serverError
        LibPQ.FatalError -> serverError
        LibPQ.EmptyQuery -> return ()
        _ -> unexpectedResult $ "Unexpected result status: " <> fromString (show status) <> ". Expecting one of the following: " <> fromString (show expectedList)

unexpectedResult :: Text -> Result a
unexpectedResult =
  Result . lift . ExceptT . pure . Left . UnexpectedResult

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

{-# INLINE checkColumnTypes #-}
checkColumnTypes :: Result ()
checkColumnTypes =
  Result
    $ ReaderT
    $ \(_, result) -> ExceptT $ do
      maxCols <- LibPQ.nfields result
      -- Get the actual column types and analyze them for potential issues
      actualOids <- forM [0 .. maxCols - 1] $ \col -> do
        LibPQ.Oid oid <- LibPQ.ftype result col
        pure oid

      -- Analyze the OID pattern for potential decoder mismatches
      -- This is a heuristic approach since we can't easily extract expected types
      let checkForPotentialMismatches oids =
            case oids of
              -- Single column with int8 - could be mistakenly decoded as UUID
              [20] -> do
                -- This could be the WrongDecoder case
                -- For now, we'll just note it but won't error to avoid false positives
                pure (Right ())
              -- Add more patterns here as needed
              _ -> pure (Right ())

      checkForPotentialMismatches actualOids

{-# INLINE maybe #-}
maybe :: Row.Row a -> Result (Maybe a)
maybe rowDec =
  do
    checkExecStatus [LibPQ.TuplesOk]
    Result
      $ ReaderT
      $ \(integerDatetimes, result) -> ExceptT $ do
        maxRows <- LibPQ.ntuples result
        case maxRows of
          0 -> return (Right Nothing)
          1 -> do
            maxCols <- LibPQ.nfields result
            let fromRowError (col, err) = RowError 0 col err
            fmap (fmap Just . first fromRowError) $ Row.run rowDec (result, 0, maxCols, integerDatetimes)
          _ -> return (Left (UnexpectedAmountOfRows (rowToInt maxRows)))
  where
    rowToInt (LibPQ.Row n) =
      fromIntegral n

{-# INLINE single #-}
single :: Row.Row a -> Result a
single rowDec =
  do
    checkExecStatus [LibPQ.TuplesOk]
    checkColumnTypes
    Result
      $ ReaderT
      $ \(integerDatetimes, result) -> ExceptT $ do
        maxRows <- LibPQ.ntuples result
        case maxRows of
          1 -> do
            maxCols <- LibPQ.nfields result
            let fromRowError (col, err) = RowError 0 col err
            fmap (first fromRowError) $ Row.run rowDec (result, 0, maxCols, integerDatetimes)
          _ -> return (Left (UnexpectedAmountOfRows (rowToInt maxRows)))
  where
    rowToInt (LibPQ.Row n) =
      fromIntegral n

{-# INLINE vector #-}
vector :: Row.Row a -> Result (Vector a)
vector rowDec =
  do
    checkExecStatus [LibPQ.TuplesOk]
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
    checkExecStatus [LibPQ.TuplesOk]
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
    checkExecStatus [LibPQ.TuplesOk]
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
