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
  )
where

import Core.Errors
import Data.Attoparsec.ByteString.Char8 qualified as Attoparsec
import Data.ByteString qualified as ByteString
import Platform.Prelude
import Pq qualified

-- | Result consumption context, for consuming a single result from a sequence of results returned by the server.
newtype ResultConsumer a
  = ResultConsumer (Pq.Result -> IO (Either ResultError a))
  deriving
    (Functor, Applicative, Monad, MonadError ResultError, MonadReader Pq.Result)
    via (ReaderT Pq.Result (ExceptT ResultError IO))

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
          maybe (Left (UnexpectedResult "No bytes")) Right
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
      Pq.BadResponse -> serverError status
      Pq.NonfatalError -> serverError status
      Pq.FatalError -> serverError status
      Pq.EmptyQuery -> return ()
      _ ->
        throwError
          ( UnexpectedResult
              ("Unexpected result status: " <> fromString (show status) <> ". Expecting one of the following: " <> fromString (show expectedList))
          )

{-# INLINE serverError #-}
serverError :: Pq.ExecStatus -> ResultConsumer ()
serverError status =
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
    pure $ Left $ ServerError (fromString (show status)) code message detail hint position
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
