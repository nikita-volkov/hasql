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
run :: Results a -> LibPQ.Connection -> Bool -> IO (Either CommandError a)
run (Results stack) conn idt =
  runExceptT (runReaderT stack (idt, conn))

{-# INLINE clientError #-}
clientError :: Results a
clientError =
  Results
    $ ReaderT
    $ \(_, connection) ->
      ExceptT
        $ fmap (Left . ClientCommandError) (LibPQ.errorMessage connection)

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
          mapLeft ResultCommandError <$> Result.run resultDec integerDatetimes result
        Nothing ->
          fmap (Left . ClientCommandError) (LibPQ.errorMessage connection)

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
              ExceptT $ fmap (mapLeft ResultCommandError) $ Result.run Result.noResult integerDatetimes result

refine :: (a -> Either Text b) -> Results a -> Results b
refine refiner (Results stack) = Results
  $ ReaderT
  $ \env -> ExceptT $ do
    resultEither <- runExceptT $ runReaderT stack env
    return $ resultEither >>= mapLeft (ResultCommandError . UnexpectedResultError) . refiner
