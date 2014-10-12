-- |
-- An open API for implementation of specific backend drivers.
module HighSQL.Backend where

import HighSQL.Prelude
import qualified Language.Haskell.TH as TH


data TransactionError =
  -- |
  -- The transaction failed and should be retried.
  TransactionConflict |
  -- |
  -- The connection got interrupted.
  Disconnected Text
  deriving (Show, Typeable)

instance Exception TransactionError


-- |
-- A width of a row and a stream of serialized values.
type ResultsStream =
  (Int, ListT IO ByteString)


class Backend b where
  -- |
  -- An argument prepared for a statement.
  data StatementArgument b
  -- |
  -- A raw value returned from the database.
  data Result b
  data Connection b
  -- |
  -- Open a connection using the backend's settings.
  connect :: b -> IO (Connection b)
  -- |
  -- Close the connection.
  disconnect :: Connection b -> IO ()
  -- |
  -- Execute a statement with values for placeholders.
  execute :: ByteString -> [StatementArgument b] -> Connection b -> IO ()
  -- |
  -- Execute a statement with values for placeholders 
  -- and an expected results stream size.
  -- The expected stream size can be used by the backend to determine 
  -- an optimal fetching method.
  executeStreaming :: ByteString -> [StatementArgument b] -> Maybe Integer -> Connection b -> IO (Int, ListT IO (Result b))
  -- |
  -- Execute a statement with values for placeholders,
  -- returning the amount of affected rows.
  executeCountingEffects :: ByteString -> [StatementArgument b] -> Connection b -> IO Integer
  -- |
  -- Start a transaction in an atomic mode if the first flag is true
  -- and in a write mode if the second one is true.
  beginTransaction :: Bool -> Bool -> Connection b -> IO ()
  -- |
  -- Finish the transaction, 
  -- while releasing all the resources acquired with 'executeStreaming'.
  --  
  -- The boolean defines whether to commit the updates,
  -- otherwise it rolls back.
  finishTransaction :: Bool -> Connection b -> IO ()


class Backend b => Mapping b v where
  renderValue :: v -> StatementArgument b
  parseResult :: Result b -> Maybe v


