module HighSQL.Statement.Model where

import HighSQL.Prelude
import qualified Database.HDBC as HDBC


data Statement =
  Statement !ByteString ![HDBC.SqlValue]
  deriving (Show, Eq)

-- |
-- "select"
newtype Select r =
  Select Statement
  deriving (Show, Eq)

-- |
-- "update", "insert", "delete"
newtype Update =
  Update Statement
  deriving (Show, Eq)

-- |
-- "create", "alter", "drop", "truncate"
newtype Create =
  Create Statement
  deriving (Show, Eq)
