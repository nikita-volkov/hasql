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

    -- ** CommandByIdt
    CommandByIdt,
    toCommandByIdt,
    fromCommandByIdt,

    -- ** RoundtripByIdt
    RoundtripByIdt,
    toRoundtripByIdt,
  )
where

import Core.Contexts.Command qualified as Command
import Core.Contexts.ResultDecoder qualified as Result
import Core.Contexts.Roundtrip qualified as Roundtrip
import Core.Errors
import Libpq qualified as Pq
import Platform.Prelude hiding (many, maybe)
import Platform.Prelude qualified as Prelude

newtype ResultsDecoder a
  = ResultsDecoder (ReaderT (Bool, Pq.Connection) (ExceptT CommandError IO) a)
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
  fromCommandByIdt \idt ->
    Command.consumeResult (Result.toResultConsumerByIdt resultDec idt)

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

type Handler a = Pq.Connection -> Bool -> IO (Either CommandError a)

toHandler :: ResultsDecoder a -> (Pq.Connection -> Bool -> IO (Either CommandError a))
toHandler (ResultsDecoder stack) =
  \connection idt ->
    runExceptT (runReaderT stack (idt, connection))

-- | Construct from a handler function.
fromHandler :: (Pq.Connection -> Bool -> IO (Either CommandError a)) -> ResultsDecoder a
fromHandler handler =
  ResultsDecoder $ ReaderT $ \(integerDatetimes, connection) ->
    ExceptT $ handler connection integerDatetimes

-- ** CommandByIdt

type CommandByIdt a = Bool -> Command.Command a

toCommandByIdt :: ResultsDecoder a -> CommandByIdt a
toCommandByIdt (ResultsDecoder stack) idt =
  Command.Command \connection -> do
    runExceptT (runReaderT stack (idt, connection))

fromCommandByIdt :: CommandByIdt a -> ResultsDecoder a
fromCommandByIdt commandByIdt = fromHandler \connection idt ->
  let (Command.Command handler) = commandByIdt idt
   in handler connection

-- ** Command

type Command = Command.Command

fromCommand :: Command a -> ResultsDecoder a
fromCommand = fromCommandByIdt . const

-- ** Roundtrip

type RoundtripByIdt a = Bool -> Roundtrip.Roundtrip a

toRoundtripByIdt :: ResultsDecoder a -> RoundtripByIdt a
toRoundtripByIdt (ResultsDecoder stack) idt =
  Roundtrip.Roundtrip \connection -> do
    pure do
      runExceptT (runReaderT stack (idt, connection))
