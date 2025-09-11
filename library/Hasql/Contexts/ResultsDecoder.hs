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
module Hasql.Contexts.ResultsDecoder
  ( Results,

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

import Hasql.Contexts.Command qualified as Command
import Hasql.Contexts.Roundtrip qualified as Roundtrip
import Hasql.Decoders.Result qualified as Result
import Hasql.Errors
import Hasql.LibPq14 qualified as Pq
import Hasql.Prelude hiding (many, maybe)
import Hasql.Prelude qualified as Prelude

newtype Results a
  = Results (ReaderT (Bool, Pq.Connection) (ExceptT CommandError IO) a)
  deriving (Functor, Applicative, Monad)

instance Filterable Results where
  {-# INLINE mapMaybe #-}
  mapMaybe fn =
    refine (Prelude.maybe (Left "Invalid result") Right . fn)

-- * Construction

-- |
-- Parse a single result.
{-# INLINE single #-}
single :: Result.Result a -> Results a
single resultDec =
  fromCommandByIdt \idt ->
    Command.consumeResult (Result.toResultConsumerByIdt resultDec idt)

{-# INLINE dropRemainders #-}
dropRemainders :: Results ()
dropRemainders =
  fromCommand Command.drainResults

refine :: (a -> Either Text b) -> Results a -> Results b
refine refiner (Results stack) = Results
  $ ReaderT
  $ \env -> ExceptT $ do
    resultEither <- runExceptT $ runReaderT stack env
    return $ resultEither >>= first (ResultError . UnexpectedResult) . refiner

-- * Relations

-- ** Handler

type Handler a = Pq.Connection -> Bool -> IO (Either CommandError a)

toHandler :: Results a -> (Pq.Connection -> Bool -> IO (Either CommandError a))
toHandler (Results stack) =
  \connection idt ->
    runExceptT (runReaderT stack (idt, connection))

-- | Construct from a handler function.
fromHandler :: (Pq.Connection -> Bool -> IO (Either CommandError a)) -> Results a
fromHandler handler =
  Results $ ReaderT $ \(integerDatetimes, connection) ->
    ExceptT $ handler connection integerDatetimes

-- ** CommandByIdt

type CommandByIdt a = Bool -> Command.Command a

toCommandByIdt :: Results a -> CommandByIdt a
toCommandByIdt (Results stack) idt =
  Command.Command \connection -> do
    runExceptT (runReaderT stack (idt, connection))

fromCommandByIdt :: CommandByIdt a -> Results a
fromCommandByIdt commandByIdt = fromHandler \connection idt ->
  let (Command.Command handler) = commandByIdt idt
   in handler connection

-- ** Command

type Command = Command.Command

fromCommand :: Command a -> Results a
fromCommand = fromCommandByIdt . const

-- ** Roundtrip

type RoundtripByIdt a = Bool -> Roundtrip.Roundtrip a

toRoundtripByIdt :: Results a -> RoundtripByIdt a
toRoundtripByIdt (Results stack) idt =
  Roundtrip.Roundtrip \connection -> do
    pure do
      runExceptT (runReaderT stack (idt, connection))
