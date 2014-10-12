-- |
-- An open API for implementation of specific backend drivers.
module HighSQL.Backend where

import HighSQL.Prelude
import qualified Language.Haskell.TH as TH


data Failure b =
  -- |
  -- A transaction failed and should be retried.
  TransactionFailure |
  -- |
  -- Cannot connect to a server 
  -- or the connection got interrupted.
  ConnectionFailure Text


type M b = 
  ExceptT (Failure b) IO


-- |
-- A width of a row and a stream of serialized values.
type ResultsStream =
  (Int, ListT IO ByteString)


class Backend b where
  -- |
  -- An argument prepared for a statement.
  type StatementArgument b
  -- |
  -- A raw value returned from the database.
  data Result b
  type Connection b
  -- |
  -- Open a connection using the backend's settings.
  connect :: b -> M b (Connection b)
  -- |
  -- Close the connection.
  disconnect :: Connection b -> M b ()
  -- |
  -- If the backend supports statement preparation,
  -- this function compiles a bytestring statement 
  -- with placeholders if it's not compiled already,
  -- and otherwise returns the cached already compiled statement. 
  -- IOW, it implements memoization.
  -- 
  -- If the backend does not support this,
  -- then this function should simply be implemented as a 'return'.
  prepare :: ByteString -> Connection b -> M b s
  -- |
  -- Execute a statement with values for placeholders.
  execute :: ByteString -> [StatementArgument b] -> Connection b -> M b ()
  -- |
  -- Execute a statement with values for placeholders 
  -- and an expected results stream size.
  -- The expected stream size can be used by the backend to determine 
  -- an optimal fetching method.
  executeStreaming :: ByteString -> [StatementArgument b] -> Maybe Integer -> Connection b -> ListT (M b) r
  -- |
  -- Execute a statement with values for placeholders,
  -- returning the amount of affected rows.
  executeCountingEffects :: s -> [StatementArgument b] -> Connection b -> M b Integer
  -- |
  -- Start a transaction in a write mode if the flag is true.
  beginTransaction :: Bool -> Connection b -> M b ()
  -- |
  -- Finish the transaction, 
  -- while releasing all the resources acquired with 'executeAndStream'.
  --  
  -- The boolean defines whether to commit the updates,
  -- otherwise it rolls back.
  finishTransaction :: Bool -> Connection b -> M b ()


class Backend b => Value v b where
  renderValue :: v -> StatementArgument b
  parseResult :: Result b -> Maybe v


