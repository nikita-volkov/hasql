module Hasql.Engine.Errors where

import Hasql.Comms.Recv qualified
import Hasql.Comms.ResultDecoder qualified
import Hasql.Comms.Roundtrip qualified
import Hasql.Comms.RowReader qualified
import Hasql.Platform.Prelude
import TextBuilder qualified

-- * Error types

-- |
-- Error that occurs when attempting to establish a database connection.
--
-- These errors can occur when calling 'Hasql.Connection.acquire'.
-- Connection errors are categorized into several types to help with
-- error handling and logging.
data AcquireError
  = -- | Network-level error while connecting to the database server.
    --
    -- This typically indicates issues like:
    --
    -- * Server is not reachable (wrong host\/port or server is down)
    -- * Network connectivity problems
    -- * Firewall blocking the connection
    -- * Connection timeout
    --
    -- These errors are transient and the operation can be retried.
    NetworkingAcquireError
      -- | Human readable details intended for logging.
      Text
  | -- | Authentication failed when connecting to the database.
    --
    -- This typically indicates issues like:
    --
    -- * Invalid username or password
    -- * User does not have permission to access the database
    -- * Authentication method mismatch (e.g., server requires SSL but client doesn't use it)
    --
    -- These errors are not transient and require fixing the credentials or permissions.
    AuthenticationAcquireError
      -- | Human readable details intended for logging.
      Text
  | -- | Compatibility issue between client and server.
    --
    -- This typically indicates issues like:
    --
    -- * Server version is too old or too new
    -- * Required PostgreSQL features are not available
    -- * Protocol version mismatch
    --
    -- These errors are not transient and require upgrading/downgrading
    -- the server or client.
    CompatibilityAcquireError
      -- | Human readable details intended for logging.
      Text
  | -- | Uncategorized error coming from @libpq@.
    --
    -- This is a catch-all for connection errors that don't fit into other categories.
    -- The error message may be empty if @libpq@ doesn't provide details.
    --
    -- These errors are not transient by default.
    OtherAcquireError
      -- | Human readable details intended for logging. May be empty.
      Text
  deriving stock (Show, Eq)

-- |
-- Error that occurs during session execution and leaves the connection
-- usable.
--
-- A session is a batch of actions executed in a database connection context.
-- Every constructor here means the connection is still fit to be handed
-- back for reuse - that is the axis 'SessionError' is narrowed on, not
-- provenance. A failure that leaves the connection gone is not a
-- 'SessionError' at all; it is one of the other two constructors of
-- 'UseError'.
--
-- Session errors provide detailed context to help diagnose problems,
-- including SQL text, parameters, and the location of the error within
-- a pipeline of statements.
data SessionError
  = -- | An error occurred while executing a statement in the session.
    --
    -- This wraps statement-level errors and provides additional context about:
    --
    -- * Which statement in the pipeline failed (when multiple statements are batched)
    -- * The SQL text and parameters of the failing statement
    -- * Whether the statement was prepared or unprepared
    --
    -- The error message includes formatted output showing all this context,
    -- making it easier to diagnose issues in production.
    StatementSessionError
      -- | Total number of statements in the running pipeline. 1 if it's executed alone.
      Int
      -- | 0-based index of the statement that failed. 0 if it's executed alone.
      Int
      -- | SQL template of the failing statement.
      Text
      -- | Parameter values as text (for logging purposes).
      [Text]
      -- | Whether the statement was executed as a prepared one.
      Bool
      -- | The underlying statement error.
      StatementError
  | -- | An error occurred while executing a script.
    --
    -- Scripts are multi-statement SQL texts executed via 'Hasql.Session.script'.
    -- Unlike regular statements, scripts don't support parameters or result decoding,
    -- and errors are limited to server-reported issues.
    ScriptSessionError
      -- | The SQL text of the script.
      Text
      -- | The server error that occurred.
      ServerError
  | -- | One or more types referenced in the statement could not be found in the database.
    --
    -- This occurs when using custom types (enums, composite types, domains) that
    -- are resolved by name at runtime, but the types don't exist in the database.
    --
    -- To fix this error:
    --
    -- * Ensure the types are defined in the database
    -- * Check that schema search paths are configured correctly
    -- * Verify that the type names in your code match those in the database
    MissingTypesSessionError
      -- | Set of (schema name, type name) pairs that could not be found.
      --
      -- Schema name is 'Nothing' when the type was looked up without a schema qualifier.
      (HashSet (Maybe Text, Text))
  deriving stock (Show, Eq)

-- |
-- Error that 'Hasql.Connection.use' can return.
--
-- 'SessionUseError' means the connection is still usable and is the only
-- constructor 'Control.Monad.Except.catchError' on a 'Hasql.Session.Session'
-- can see. 'ConnectionUseError' means the connection is gone, and no handler
-- inside the session can intercept it - the split is structural, enforced by
-- the 'Session' 'Control.Monad.Monad' instance rather than by any check a
-- handler has to remember to make.
data UseError
  = -- | The session reported an ordinary failure and the connection is
    -- still usable.
    SessionUseError
      SessionError
  | -- | The connection is gone.
    --
    -- This covers every way 'Hasql.Connection.use' can lose the connection:
    -- a dropped socket, a request @libpq@ refused to send outright (e.g.
    -- more than 65535 parameters in one statement), an unexpected response
    -- from the server, or a bug in Hasql. The reason text says which.
    --
    -- 'Hasql.Connection.use' has finished the connection before returning
    -- this, so the 'Hasql.Connection.Connection' the session ran on is
    -- spent: further 'Hasql.Connection.use' on it fails with this same
    -- error, and 'Hasql.Connection.release' is a no-op. Pools must discard
    -- it rather than return it.
    --
    -- Whatever the cause, the driver cannot vouch for the protocol state of
    -- a connection a request failed to leave, and repairing one costs more
    -- than replacing it: the repair would have to push a Sync, which
    -- flushes and commits the very commands the failed session never got to
    -- complete. So this is reported uniformly rather than split by cause -
    -- retry policy belongs to the caller, informed by the reason text and,
    -- for server errors, 'Hasql.Errors.toSqlState'.
    ConnectionUseError
      -- | Human-readable details about what went wrong.
      Text
  deriving stock (Show, Eq)

-- |
-- Error that occurs when executing a single statement.
--
-- Statement errors can be caused by server-side issues (SQL errors, constraint violations)
-- or by mismatches between the decoder specification and the actual result structure
-- (wrong number of rows/columns, type mismatches, or cell-level decoding failures).
data StatementError
  = -- | The server rejected the statement and returned an error.
    --
    -- This includes SQL syntax errors, constraint violations, permission errors,
    -- and any other error reported by PostgreSQL during statement execution.
    ServerStatementError ServerError
  | -- | The statement returned a different number of rows than expected.
    --
    -- This occurs when using result decoders like @Decoders.singleRow@ or
    -- @Decoders.rowsAffectedAtLeast@ that have specific row count expectations.
    UnexpectedRowCountStatementError
      -- | Expected minimum number of rows.
      Int
      -- | Expected maximum number of rows.
      Int
      -- | Actual number of rows returned.
      Int
  | -- | The statement returned a different number of columns than expected.
    --
    -- This indicates a mismatch between the decoder specification and the
    -- actual result structure, possibly due to:
    --
    -- * Schema changes (columns added or removed)
    -- * Wrong query (selecting different columns than expected)
    -- * Decoder configuration error
    UnexpectedColumnCountStatementError
      -- | Expected number of columns.
      Int
      -- | Actual number of columns returned.
      Int
  | -- | A column has a different type than expected.
    --
    -- This occurs when the decoder expects a specific PostgreSQL type (by OID)
    -- but the actual column has a different type. This can happen due to:
    --
    -- * Schema changes (column type changed)
    -- * Wrong query (selecting wrong column or using a cast)
    -- * Decoder configuration error
    --
    -- Note: As of version 1.10, Hasql performs strict type checking and will
    -- report this error instead of attempting automatic type coercion.
    UnexpectedColumnTypeStatementError
      -- | 0-based column index where the type mismatch occurred.
      Int
      -- | Expected PostgreSQL type OID.
      Word32
      -- | Actual PostgreSQL type OID of the column.
      Word32
  | -- | An error occurred while decoding a specific row.
    --
    -- This wraps errors that occur at the row or cell level, providing
    -- context about which row failed.
    RowStatementError
      -- | 0-based index of the row that failed to decode.
      Int
      -- | The underlying row-level error.
      RowError
  | -- | The database returned an unexpected result structure.
    --
    -- This is a catch-all error that indicates either:
    --
    -- * An improper statement (e.g., executing a query that doesn't match expectations)
    -- * A schema mismatch between the code and database
    -- * A bug in Hasql or server misbehavior
    UnexpectedResultStatementError
      -- | Human-readable details about what went wrong.
      Text
  deriving stock (Show, Eq)

-- |
-- Error reported by the PostgreSQL server when executing a statement.
--
-- The server provides structured error information including error codes
-- (SQL state), messages, and optional context like hints and position information.
--
-- For a complete list of PostgreSQL error codes, see:
-- <https://www.postgresql.org/docs/current/errcodes-appendix.html>
data ServerError
  = ServerError
      -- | SQL State Code (SQLSTATE).
      --
      -- A five-character code that identifies the error class and condition.
      -- Examples: @\"23505\"@ (unique violation), @\"42P01\"@ (undefined table).
      Text
      -- | Primary error message.
      --
      -- A human-readable description of the error.
      Text
      -- | Optional detailed error information.
      --
      -- Additional context about the error, if available.
      (Maybe Text)
      -- | Optional hint for resolving the error.
      --
      -- A suggestion for how to fix the problem, if available.
      (Maybe Text)
      -- | Optional position in the SQL string where the error occurred.
      --
      -- A 1-based character index into the SQL string, if the error
      -- relates to a specific location in the query.
      (Maybe Int)
  deriving stock (Show, Eq)

-- |
-- Error that occurs when decoding a result row.
--
-- Row errors indicate problems when processing an individual row from the result set,
-- either at the cell level or during row refinement/validation.
data RowError
  = -- | An error occurred while decoding a specific cell in the row.
    --
    -- This wraps cell-level errors (null handling, deserialization failures)
    -- and provides context about which column failed and its type.
    CellRowError
      -- | 0-based index of the column where the error occurred.
      Int
      -- | PostgreSQL type OID of the column, as reported by the server.
      Word32
      -- | The underlying cell-level error.
      CellError
  | -- | A refinement or validation error when processing the row.
    --
    -- This occurs when using refinement functions in row decoders
    -- (e.g., with 'Decoders.refineRow') to validate or transform
    -- decoded values. The refinement function rejected the row data.
    RefinementRowError
      -- | Human-readable details about why the refinement failed.
      Text
  deriving stock (Show, Eq)

-- |
-- Error that occurs when decoding a single cell (column value) in a result row.
--
-- Cell errors indicate problems with individual values returned by the database,
-- such as unexpected nulls or failures in binary deserialization.
data CellError
  = -- | A NULL value was encountered when a non-NULL value was expected.
    --
    -- This occurs when using non-nullable decoders (e.g., @Decoders.nonNullable@)
    -- on a column that contains a NULL value.
    UnexpectedNullCellError
  | -- | Failed to deserialize the cell value from its binary representation.
    --
    -- This can occur when:
    --
    -- * The binary data is corrupted
    -- * The decoder doesn't match the actual data type
    -- * The data format is invalid for the expected type
    DeserializationCellError
      -- | Human-readable error message describing what went wrong.
      Text
  deriving stock (Show, Eq)

-- |
-- Whether a round trip error is a @Parse@ failing with SQLSTATE @42P05@
-- ("prepared statement \"...\" already exists").
--
-- Since prepared statement names are content-addressed (a SHA-256 digest of
-- the SQL text and the parameter OIDs), a collision on the name is - up to a
-- hash collision - a collision on the statement itself: whatever the server
-- already holds under that name is the very statement that was about to be
-- parsed. Callers that evict a cache entry on ordinary @Parse@ failures
-- should keep it in this one case, since the statement is in fact on the
-- server under the name they just tried to commit.
--
-- This is a fact about Hasql's own naming scheme, not something derivable
-- from the PostgreSQL error-code appendix: nothing about @42P05@ in general
-- says a collision is safe to treat as a cache hit, only Hasql's
-- content-addressing does.
isPrepareCollision :: Hasql.Comms.Roundtrip.Error tag -> Bool
isPrepareCollision = \case
  Hasql.Comms.Roundtrip.ServerError (Hasql.Comms.Recv.ResultError _ _ (Hasql.Comms.ResultDecoder.ServerError code _ _ _ _)) ->
    code == "42P05"
  _ -> False

-- | Turn a failed @libpq@ send into a 'UseError'.
--
-- A failed send always finishes the connection: either the socket is gone,
-- or @libpq@ refused the request with commands possibly queued behind it and
-- no way to tell what the server has seen. Neither is worth repairing - a
-- repair would have to push a Sync, which flushes and commits the very
-- commands the failed session never got to complete - so the connection is
-- finished instead.
fromSendError :: Text -> UseError
fromSendError = ConnectionUseError

fromRoundtripError :: Hasql.Comms.Roundtrip.Error tag -> UseError
fromRoundtripError = \case
  Hasql.Comms.Roundtrip.ClientError _tag details ->
    fromSendError details
  Hasql.Comms.Roundtrip.ServerError recvError ->
    fromRecvError (Nothing <$ recvError)

fromRecvError :: Hasql.Comms.Recv.Error (Maybe (Int, Int, ByteString, [Text], Bool)) -> UseError
fromRecvError = \case
  Hasql.Comms.Recv.ResultError tag _resultOffset resultError ->
    case tag of
      Nothing ->
        (ConnectionUseError . TextBuilder.toText . mconcat)
          [ "Unexpected error in an operation not associated with any statement. ",
            "This indicates a bug in Hasql or the server misbehaving. ",
            "Error: ",
            TextBuilder.string (show resultError)
          ]
      Just (totalStatements, statementIndex, sql, parameters, prepared) ->
        SessionUseError
          $ StatementSessionError
            totalStatements
            statementIndex
            (decodeUtf8Lenient sql)
            parameters
            prepared
            (fromStatementResultError resultError)
  Hasql.Comms.Recv.NoResultsError tag connectionLost details ->
    if connectionLost
      then -- The results are not late, they are never coming: the socket is
      -- gone. Reporting this as a row-count mismatch would describe a
      -- dropped connection as a statement that returned nothing, and
      -- leave pools reusing a connection that no longer exists.
        ConnectionUseError (maybe "" decodeUtf8Lenient details)
      else case tag of
        Nothing ->
          (ConnectionUseError . TextBuilder.toText . mconcat)
            [ "Unexpectedly got no results in an operation not associated with any statement. ",
              "This indicates a bug in Hasql or the server misbehaving. ",
              "Details: ",
              TextBuilder.string (show details)
            ]
        Just (totalStatements, statementIndex, sql, parameters, prepared) ->
          SessionUseError
            $ StatementSessionError
              totalStatements
              statementIndex
              (decodeUtf8Lenient sql)
              parameters
              prepared
              (UnexpectedRowCountStatementError 1 1 0)
  Hasql.Comms.Recv.TooManyResultsError tag actual ->
    case tag of
      Nothing ->
        (ConnectionUseError . TextBuilder.toText . mconcat)
          [ "Unexpectedly got too many results in an operation not associated with any statement. ",
            "This indicates a bug in Hasql or the server misbehaving. ",
            "Amount: ",
            TextBuilder.decimal actual
          ]
      Just (totalStatements, statementIndex, sql, parameters, prepared) ->
        SessionUseError
          $ StatementSessionError
            totalStatements
            statementIndex
            (decodeUtf8Lenient sql)
            parameters
            prepared
            (UnexpectedRowCountStatementError 1 1 actual)

fromStatementResultError :: Hasql.Comms.ResultDecoder.Error -> StatementError
fromStatementResultError = \case
  Hasql.Comms.ResultDecoder.ServerError code message detail hint position ->
    ServerStatementError
      ( ServerError
          (decodeUtf8Lenient code)
          (decodeUtf8Lenient message)
          (fmap decodeUtf8Lenient detail)
          (fmap decodeUtf8Lenient hint)
          position
      )
  Hasql.Comms.ResultDecoder.UnexpectedResult msg ->
    UnexpectedResultStatementError msg
  Hasql.Comms.ResultDecoder.UnexpectedRowCount actual ->
    UnexpectedRowCountStatementError 1 1 actual
  Hasql.Comms.ResultDecoder.UnexpectedColumnCount expected actual ->
    UnexpectedColumnCountStatementError expected actual
  Hasql.Comms.ResultDecoder.DecoderTypeMismatch colIdx expectedOid actualOid ->
    UnexpectedColumnTypeStatementError
      colIdx
      expectedOid
      actualOid
  Hasql.Comms.ResultDecoder.RowError rowIndex rowError ->
    case rowError of
      Hasql.Comms.RowReader.CellError column oid cellErr ->
        RowStatementError
          rowIndex
          ( CellRowError
              column
              oid
              ( case cellErr of
                  Hasql.Comms.RowReader.DecodingCellError msg -> DeserializationCellError msg
                  Hasql.Comms.RowReader.UnexpectedNullCellError -> UnexpectedNullCellError
              )
          )
      Hasql.Comms.RowReader.RefinementError msg ->
        RowStatementError
          rowIndex
          (RefinementRowError msg)

fromRecvErrorInScript :: ByteString -> Hasql.Comms.Recv.Error (Maybe ByteString) -> UseError
fromRecvErrorInScript scriptSql = \case
  Hasql.Comms.Recv.ResultError _ _ resultError ->
    case resultError of
      Hasql.Comms.ResultDecoder.ServerError code message detail hint position ->
        SessionUseError
          $ ScriptSessionError
            (decodeUtf8Lenient scriptSql)
            ( ServerError
                (decodeUtf8Lenient code)
                (decodeUtf8Lenient message)
                (fmap decodeUtf8Lenient detail)
                (fmap decodeUtf8Lenient hint)
                position
            )
      Hasql.Comms.ResultDecoder.UnexpectedResult msg ->
        (ConnectionUseError . TextBuilder.toText . mconcat)
          [ "Unexpected result in script: ",
            TextBuilder.text msg
          ]
      Hasql.Comms.ResultDecoder.UnexpectedRowCount actual ->
        (ConnectionUseError . TextBuilder.toText . mconcat)
          [ "Unexpected amount of rows in script: ",
            TextBuilder.decimal actual
          ]
      Hasql.Comms.ResultDecoder.UnexpectedColumnCount expected actual ->
        (ConnectionUseError . TextBuilder.toText . mconcat)
          [ "Unexpected amount of columns in script: expected ",
            TextBuilder.decimal expected,
            ", got ",
            TextBuilder.decimal actual
          ]
      Hasql.Comms.ResultDecoder.DecoderTypeMismatch colIdx expectedOid actualOid ->
        (ConnectionUseError . TextBuilder.toText . mconcat)
          [ "Decoder type mismatch in script: expected OID ",
            TextBuilder.string (show expectedOid),
            " at column ",
            TextBuilder.decimal colIdx,
            ", got ",
            TextBuilder.string (show actualOid)
          ]
      Hasql.Comms.ResultDecoder.RowError rowIndex rowError ->
        (ConnectionUseError . TextBuilder.toText . mconcat)
          [ "Row error in script at row ",
            TextBuilder.decimal rowIndex,
            ": ",
            TextBuilder.string (show rowError)
          ]
  Hasql.Comms.Recv.NoResultsError _ connectionLost details ->
    if connectionLost
      then ConnectionUseError (maybe "" decodeUtf8Lenient details)
      else
        (ConnectionUseError . TextBuilder.toText . mconcat)
          [ "Got no results in script.",
            " This indicates a bug in Hasql or the server misbehaving.",
            details
              & filter (/= "")
              & foldMap (mappend " Details: " . TextBuilder.text . decodeUtf8Lenient)
          ]
  Hasql.Comms.Recv.TooManyResultsError _ actual ->
    (ConnectionUseError . TextBuilder.toText . mconcat)
      [ "Got too many results in script. ",
        "This indicates a bug in Hasql or the server misbehaving. ",
        "Amount: ",
        TextBuilder.decimal actual
      ]
