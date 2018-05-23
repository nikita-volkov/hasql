-- |
-- An API for retrieval of multiple results.
-- Can be used to handle:
-- 
-- * A single result,
-- 
-- * Individual results of a multi-statement query
-- with the help of "Applicative" and "Monad",
-- 
-- * Row-by-row fetching.
-- 
module Hasql.Private.Decoders.Results where

import Hasql.Private.Prelude hiding (maybe, many)
import Hasql.Private.Errors
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Hasql.Private.Prelude as Prelude
import qualified Hasql.Private.Decoders.Result as Result
import qualified Hasql.Private.Decoders.Row as Row


newtype Results a =
  Results (ReaderT (Bool, LibPQ.Connection) (ExceptT CommandError IO) a)
  deriving (Functor, Applicative, Monad)


{-# INLINE run #-}
run :: Results a -> (Bool, LibPQ.Connection) -> IO (Either CommandError a)
run (Results stack) env =
  runExceptT (runReaderT stack env)

{-# INLINE clientError #-}
clientError :: Results a
clientError =
  Results $ ReaderT $ \(_, connection) -> ExceptT $
  fmap (Left . ClientError) (LibPQ.errorMessage connection)

-- |
-- Parse a single result.
{-# INLINE single #-}
single :: Result.Result a -> Results a
single resultDec =
  Results $ ReaderT $ \(integerDatetimes, connection) -> ExceptT $ do
    resultMaybe <- LibPQ.getResult connection
    case resultMaybe of
      Just result ->
        mapLeft ResultError <$> Result.run resultDec (integerDatetimes, result) 
      Nothing ->
        fmap (Left . ClientError) (LibPQ.errorMessage connection)

-- |
-- Fetch a single result.
{-# INLINE getResult #-}
getResult :: Results LibPQ.Result
getResult =
  Results $ ReaderT $ \(_, connection) -> ExceptT $ do
    resultMaybe <- LibPQ.getResult connection
    case resultMaybe of
      Just result -> pure (Right result)
      Nothing -> fmap (Left . ClientError) (LibPQ.errorMessage connection)

-- |
-- Fetch a single result.
{-# INLINE getResultMaybe #-}
getResultMaybe :: Results (Maybe LibPQ.Result)
getResultMaybe =
  Results $ ReaderT $ \(_, connection) -> lift $ LibPQ.getResult connection

{-# INLINE dropRemainders #-}
dropRemainders :: Results ()
dropRemainders =
  {-# SCC "dropRemainders" #-} 
  Results $ ReaderT $ \(integerDatetimes, connection) -> loop integerDatetimes connection
  where
    loop integerDatetimes connection =
      getResultMaybe >>= Prelude.maybe (pure ()) onResult
      where
        getResultMaybe =
          lift $ LibPQ.getResult connection
        onResult result =
          loop integerDatetimes connection <* checkErrors
          where
            checkErrors =
              ExceptT $ fmap (mapLeft ResultError) $ Result.run Result.unit (integerDatetimes, result)
