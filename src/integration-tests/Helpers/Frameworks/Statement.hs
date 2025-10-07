module Helpers.Frameworks.Statement where

import Hasql.Statement qualified as Statement

class StatementModule params result | params -> result where
  statement :: Statement.Statement params result
