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
module Hasql.Decoders.Results where

import Database.PostgreSQL.LibPQ qualified as LibPQ
import Hasql.Decoders.Result qualified as Result
import Hasql.Errors
import Hasql.Prelude hiding (many, maybe)
import Hasql.Prelude qualified as Prelude

newtype Results a
  = Results (ReaderT (Bool, LibPQ.Connection) (ExceptT CommandError IO) a)
  deriving (Functor, Applicative, Monad)

{-# INLINE run #-}
run :: Results a -> (Bool, LibPQ.Connection) -> IO (Either CommandError a)
run (Results stack) env =
  runExceptT (runReaderT stack env)

{-# INLINE clientError #-}
clientError :: Results a
clientError =
  Results
    $ ReaderT
    $ \(_, connection) ->
      ExceptT
        $ fmap (Left . ClientError) (LibPQ.errorMessage connection)

-- |
-- Parse a single result.
{-# INLINE single #-}
single :: Result.Result a -> Results a
single resultDec =
  Results
    $ ReaderT
    $ \(integerDatetimes, connection) -> ExceptT $ do
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
  Results
    $ ReaderT
    $ \(_, connection) -> ExceptT $ do
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
              ExceptT $ fmap (mapLeft ResultError) $ Result.run Result.noResult (integerDatetimes, result)

refine :: (a -> Either Text b) -> Results a -> Results b
refine refiner results = Results
  $ ReaderT
  $ \env -> ExceptT $ do
    resultEither <- run results env
    return $ resultEither >>= mapLeft (ResultError . UnexpectedResult) . refiner
