-- |
-- Explicit error types for all Hasql operations.
--
-- This module provides access to all error types used throughout Hasql:
--
-- * 'ConnectionError' - errors that occur when establishing a database connection
-- * 'SessionError' - errors that occur during session execution
--
-- The module follows Hasql's philosophy of explicit error handling,
-- where all errors are represented as values rather than exceptions.
module Hasql.Errors
  ( -- * Connection errors
    ConnectionError (..),

    -- * Session errors
    SessionError (..),
    StatementError (..),
    CellError (..),
    ExecutionError (..),
  )
where

import Core.Errors
  ( CellError (..),
    ConnectionError (..),
    ExecutionError (..),
    SessionError (..),
    StatementError (..),
  )
