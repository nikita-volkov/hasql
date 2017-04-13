module Hasql.Connection.Session.Session
where

import Hasql.Prelude
import qualified Hasql.Client.Communicator as A
import qualified Hasql.Client.Model as C
import qualified Hasql.Connection.Session.Statement.Statement as B
import qualified Hasql.PreparedStatementRegistry as D
import qualified Data.Vector as E


data Env =
  Env A.Communicator {-# UNPACK #-} !C.BackendSettings D.PreparedStatementRegistry

newtype Session result =
  Session (Env -> IO (Either C.Error result))

deriving instance Functor Session

instance Applicative Session where
  {-# INLINE pure #-}
  pure =
    Session . const . pure . pure
  {-# INLINABLE (<*>) #-}
  (<*>) (Session leftIO) (Session rightIO) =
    Session $ \env -> do
      leftEither <- leftIO env
      case leftEither of
        Left error ->
          return (Left error)
        Right leftResult -> 
          do
            rightEither <- rightIO env
            return (fmap leftResult rightEither)

instance Monad Session where
  {-# INLINE return #-}
  return =
    pure
  {-# INLINABLE (>>=) #-}
  (>>=) (Session leftIO) rightSession =
    Session $ \env -> runExceptT $ do
      leftResult <- ExceptT (leftIO env)
      case rightSession leftResult of
        Session rightIO -> ExceptT (rightIO env)

{-# INLINABLE batch #-}
batch :: Batch result -> Session result
batch (Batch batchFuture) =
  Session $ \env@(Env communicator _ _) -> do
    batchIO <- batchFuture env
    syncIO <- A.sync communicator
    flushEitherRef <- newIORef (Right ())
    flushIO <- A.flush communicator (writeIORef flushEitherRef . Left . C.TransportError)
    batchEither <- batchIO
    syncEither <- syncIO
    flushEither <- readIORef flushEitherRef
    return (batchEither <* syncEither <* flushEither)

{-|
One peculiarity of the batch execution is that it does not allow for one statement
to depend on the result of another statement from the same batch.
This is why this abstraction does not have an instance of `Monad` and only has `Applicative`.

If you need to collect the results of one statement before executing the other,
execute them in separate batches.
-}
newtype Batch result =
  Batch (Env -> IO (IO (Either C.Error result)))

deriving instance Functor Batch

instance Applicative Batch where
  {-# INLINE pure #-}
  pure =
    Batch . const . pure . pure . pure
  {-# INLINABLE (<*>) #-}
  (<*>) (Batch leftFuture) (Batch rightFuture) =
    Batch $ \env ->
    do
      leftIO <- leftFuture env
      rightIO <- rightFuture env
      return $ do
        leftEither <- leftIO
        rightEither <- rightIO
        return $ do
          leftResult <- leftEither
          rightResult <- rightEither
          return (leftResult rightResult)

statement :: B.Statement params result -> params -> Batch result
statement (B.Statement template paramOIDs paramBytesBuilder1 paramBytesBuilder2 resultColumnsAmount resultOIDsValidator resultCollector1 resultCollector2 prepared) params =
  Batch io
  where
    io (Env communicator (C.BackendSettings integerDateTimes) preparedStatementRegistry) =
      do
        traceMarkerIO ("statement " <> show template)
        (key, syncParse) <- parseAndResolveStatementName
        syncBind <- A.bindEncoded communicator "" key ((fromIntegral . E.length) paramOIDs) (paramBytesBuilder params)
        syncExecute <- A.execute communicator "" resultCollector
        return $ do
          parseEither <- syncParse
          bindEither <- syncBind
          executeEither <- syncExecute
          return (parseEither *> bindEither *> executeEither)
      where
        parseAndResolveStatementName =
          case prepared of
            True ->
              D.update preparedStatementRegistry (D.LocalKey template paramOIDs) prepare existingKeyCont
              where
                prepare newKey =
                  do
                    sync <- A.parse communicator newKey template paramOIDs
                    return (True, (newKey, sync))
                existingKeyCont key =
                  return (key, return (Right ()))
            False ->
              do
                sync <- A.parse communicator "" template paramOIDs
                return ("", sync)
        (paramBytesBuilder, resultCollector) =
          bool (paramBytesBuilder2, resultCollector2) (paramBytesBuilder1, resultCollector1) integerDateTimes
