module TestingKit.Statements.ErrorGenerators where

import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import TestingKit.Preludes.Base

-- | Statements that generate specific types of errors for testing

-- * ServerError generators

syntaxErrorStatement :: Bool -> Statement.Statement () ()
syntaxErrorStatement preparable =
  Statement.Statement
    "INVALID SQL SYNTAX HERE"
    Encoders.noParams
    Decoders.noResult
    preparable

relationNotFoundStatement :: Bool -> Statement.Statement () [Int32]
relationNotFoundStatement preparable =
  Statement.Statement
    "SELECT * FROM nonexistent_table_xyz"
    Encoders.noParams
    (Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.int4)))
    preparable

columnNotFoundStatement :: Bool -> Statement.Statement () [Int32]
columnNotFoundStatement preparable =
  Statement.Statement
    "SELECT nonexistent_column FROM generate_series(1,1)"
    Encoders.noParams
    (Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.int4)))
    preparable

divisionByZeroStatement :: Bool -> Statement.Statement () Int32
divisionByZeroStatement preparable =
  Statement.Statement
    "SELECT 1/0"
    Encoders.noParams
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
    preparable

numericOutOfRangeStatement :: Bool -> Statement.Statement () Int32
numericOutOfRangeStatement preparable =
  Statement.Statement
    "SELECT 999999999999999999999999999999999::int4"
    Encoders.noParams
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
    preparable

-- * DecoderTypeMismatch generators

textAsIntStatement :: Bool -> Statement.Statement () Int32
textAsIntStatement preparable =
  Statement.Statement
    "SELECT 'hello'"
    Encoders.noParams
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
    preparable

intAsUUIDStatement :: Bool -> Statement.Statement () UUID
intAsUUIDStatement preparable =
  Statement.Statement
    "SELECT 42"
    Encoders.noParams
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.uuid)))
    preparable

nullAsNonNullableStatement :: Bool -> Statement.Statement () Int32
nullAsNonNullableStatement preparable =
  Statement.Statement
    "SELECT NULL"
    Encoders.noParams
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
    preparable

-- * UnexpectedAmountOfRows generators

multipleRowsAsSingleStatement :: Bool -> Statement.Statement () Int32
multipleRowsAsSingleStatement preparable =
  Statement.Statement
    "SELECT generate_series FROM generate_series(1,3)"
    Encoders.noParams
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
    preparable

noRowsAsSingleStatement :: Bool -> Statement.Statement () Int32
noRowsAsSingleStatement preparable =
  Statement.Statement
    "SELECT 1 WHERE FALSE"
    Encoders.noParams
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
    preparable

-- * UnexpectedAmountOfColumns generators

oneColumnAsTwoStatement :: Bool -> Statement.Statement () (Int32, Int32)
oneColumnAsTwoStatement preparable =
  Statement.Statement
    "SELECT 1"
    Encoders.noParams
    ( Decoders.singleRow
        ( (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.int4)
            <*> Decoders.column (Decoders.nonNullable Decoders.int4)
        )
    )
    preparable

threeColumnsAsOneStatement :: Bool -> Statement.Statement () Int32
threeColumnsAsOneStatement preparable =
  Statement.Statement
    "SELECT 1, 2, 3"
    Encoders.noParams
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
    preparable

-- * Parametric error generators

data DivisionParams = DivisionParams
  { dividend :: Int32,
    divisor :: Int32
  }

parametricDivisionStatement :: Bool -> Statement.Statement DivisionParams Int32
parametricDivisionStatement preparable =
  Statement.Statement
    "SELECT $1 / $2"
    ( mconcat
        [ dividend >$< Encoders.param (Encoders.nonNullable Encoders.int4),
          divisor >$< Encoders.param (Encoders.nonNullable Encoders.int4)
        ]
    )
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
    preparable

data TypeMismatchParams = TypeMismatchParams
  { inputText :: ByteString
  }

parametricTypeMismatchStatement :: Bool -> Statement.Statement TypeMismatchParams Int32
parametricTypeMismatchStatement preparable =
  Statement.Statement
    "SELECT $1::int4"
    (inputText >$< Encoders.param (Encoders.nonNullable Encoders.bytea))
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
    preparable

-- * Convenience functions for sessions and pipelines

runSyntaxError :: Bool -> Session.Session ()
runSyntaxError preparable = Session.statement () (syntaxErrorStatement preparable)

runRelationNotFound :: Bool -> Session.Session [Int32]
runRelationNotFound preparable = Session.statement () (relationNotFoundStatement preparable)

runDivisionByZero :: Bool -> Session.Session Int32
runDivisionByZero preparable = Session.statement () (divisionByZeroStatement preparable)

runParametricDivision :: Bool -> DivisionParams -> Session.Session Int32
runParametricDivision preparable params = Session.statement params (parametricDivisionStatement preparable)
