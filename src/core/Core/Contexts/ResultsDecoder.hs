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
module Core.Contexts.ResultsDecoder
  ( ResultsDecoder,

    -- * Refinement
    refine,

    -- * Constructors
    single,
    dropRemainders,

    -- * Relations

    -- ** Handler
    Handler,
    toHandler,
    fromHandler,

    -- ** Command
    Command,
    toCommand,
    fromCommand,

    -- ** Roundtrip
    Roundtrip,
    toRoundtrip,

    -- * Classes
    Wraps (..),
  )
where

import Core.Contexts.Command qualified as Command
import Core.Contexts.ResultDecoder qualified as Result
import Core.Contexts.Roundtrip qualified as Roundtrip
import Core.Errors
import Platform.Prelude hiding (many, maybe)
import Platform.Prelude qualified as Prelude
import Pq qualified

newtype ResultsDecoder a
  = ResultsDecoder (ReaderT Pq.Connection (ExceptT CommandError IO) a)
  deriving (Functor, Applicative, Monad)

instance Filterable ResultsDecoder where
  {-# INLINE mapMaybe #-}
  mapMaybe fn =
    refine (Prelude.maybe (Left "Invalid result") Right . fn)

-- * Construction

-- |
-- Parse a single result.
{-# INLINE single #-}
single :: Result.ResultDecoder a -> ResultsDecoder a
single resultDec =
  fromCommand
    $ Command.consumeResult (Result.toResultConsumer resultDec)

{-# INLINE dropRemainders #-}
dropRemainders :: ResultsDecoder ()
dropRemainders =
  fromCommand Command.drainResults

refine :: (a -> Either Text b) -> ResultsDecoder a -> ResultsDecoder b
refine refiner (ResultsDecoder stack) = ResultsDecoder
  $ ReaderT
  $ \env -> ExceptT $ do
    resultEither <- runExceptT $ runReaderT stack env
    return $ resultEither >>= first (ResultError . UnexpectedResult) . refiner

-- * Relations

-- ** Handler

type Handler a = Pq.Connection -> IO (Either CommandError a)

toHandler :: ResultsDecoder a -> (Pq.Connection -> IO (Either CommandError a))
toHandler (ResultsDecoder stack) =
  \connection ->
    runExceptT (runReaderT stack connection)

-- | Construct from a handler function.
fromHandler :: (Pq.Connection -> IO (Either CommandError a)) -> ResultsDecoder a
fromHandler handler =
  ResultsDecoder $ ReaderT $ \connection ->
    ExceptT $ handler connection

-- ** Command

type Command a = Command.Command a

toCommand :: ResultsDecoder a -> Command a
toCommand (ResultsDecoder stack) =
  Command.Command \connection -> do
    runExceptT (runReaderT stack connection)

fromCommand :: Command a -> ResultsDecoder a
fromCommand command = fromHandler \connection ->
  let (Command.Command handler) = command
   in handler connection

-- ** Roundtrip

type Roundtrip a = Roundtrip.Roundtrip a

toRoundtrip :: ResultsDecoder a -> Roundtrip a
toRoundtrip (ResultsDecoder stack) =
  Roundtrip.Roundtrip \connection -> do
    pure do
      runExceptT (runReaderT stack connection)

-- * Classes

class Wraps f where
  wrap :: ResultsDecoder a -> f a
  unwrap :: f a -> ResultsDecoder a
