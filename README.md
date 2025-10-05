# Hasql

[![Hackage](https://img.shields.io/hackage/v/hasql.svg)](https://hackage.haskell.org/package/hasql)
[![Continuous Haddock](https://img.shields.io/badge/haddock-master-blue)](https://nikita-volkov.github.io/hasql/)

PostgreSQL driver for Haskell, that prioritizes:

- Performance
- Typesafety
- Flexibility

# Status

Hasql is production-ready, actively maintained and the API is moderately stable. It's used by many companies and most notably by the [Postgrest](https://github.com/PostgREST/postgrest) project.

# Discussions

Join [GitHub Discussions](https://github.com/nikita-volkov/hasql/discussions) to ask questions, provide feedback, suggest and vote on features, and help shape the future of Hasql.

# Ecosystem

Hasql is not just a single library, it is a granular ecosystem of composable libraries, each isolated to perform its own task and stay simple.

- ["hasql"](https://github.com/nikita-volkov/hasql) - the root of the ecosystem, which provides the essential abstraction over the PostgreSQL client functionality and mapping of values. Everything else revolves around that library.

- ["hasql-th"](https://github.com/nikita-volkov/hasql-th) - Template Haskell utilities, providing compile-time syntax checking and easy statement declaration. 

- ["hasql-transaction"](https://github.com/nikita-volkov/hasql-transaction) - an STM-inspired composable abstraction over database transactions providing automated conflict resolution.

- ["hasql-dynamic-statements"](https://github.com/nikita-volkov/hasql-dynamic-statements) - a toolkit for generating statements based on the parameters.

- ["hasql-cursor-query"](https://github.com/nikita-volkov/hasql-cursor-query) - a declarative abstraction over cursors.

- ["hasql-cursor-transaction"](https://github.com/nikita-volkov/hasql-cursor-transaction) - a lower-level abstraction over cursors, which however allows to fetch from multiple cursors simultaneously. Generally though "hasql-cursor-query" is the recommended alternative.

- ["hasql-pool"](https://github.com/nikita-volkov/hasql-pool) - a Hasql-specialized abstraction over the connection pool.

- ["hasql-migration"](https://github.com/tvh/hasql-migration) - A port of postgresql-simple-migration for use with hasql.

- ["hasql-listen-notify"](https://github.com/awkward-squad/hasql-listen-notify) / ["hasql-notifications"](https://github.com/diogob/hasql-notifications) - Support for PostgreSQL asynchronous notifications.

- ["hasql-optparse-applicative"](https://github.com/sannsyn/hasql-optparse-applicative) - "optparse-applicative" parsers for Hasql.

- ["hasql-implicits"](https://github.com/nikita-volkov/hasql-implicits) - implicit definitions, such as default codecs for standard types.

- ["hasql-interpolate"](https://github.com/awkward-squad/hasql-interpolate) - a QuasiQuoter that supports interpolating Haskell expressions into Hasql queries.

<small>Want to list your package or correct something here? Make a PR.</small>

## Why make it an ecosystem?

- **Focus.**
Each library in isolation provides a simple API, which is focused on a specific task or a few related tasks.

- **Flexibility.**
The user picks and chooses the features, thus precisely matching the level of abstraction that he needs for his task.

- **Much more stable and descriptive semantic versioning.**
E.g., a change in the API of the "hasql-transaction" library won't affect any of the other libraries and it gives the user a more precise information about which part of his application he needs to update to conform.

- **Interchangeability and competition of the ecosystem components.**
E.g., [not everyone will agree](https://github.com/nikita-volkov/hasql/issues/41) with the restrictive design decisions made in the "hasql-transaction" library. However those decisions are not imposed on the user, and instead of having endless debates about how to abstract over transactions, another extension library can simply be released, which will provide a different interpretation of what the abstraction over transactions should be.

- **Horizontal scalability of the ecosystem.**
Instead of posting feature- or pull-requests, the users are encouraged to release their own small extension-libraries, with themselves becoming the copyright owners and taking on the maintenance responsibilities. Compare this model to the classical one, where some core-team is responsible for everything. One is scalable, the other is not.

# Tutorials

## Videos

There's several videos on Hasql done as part of a nice intro-level series of live Haskell+Bazel coding by the "Ants Are Everywhere" YouTube channel:

- [Coding Day 20: Switching from postgresql-simple to Hasql](https://youtu.be/ce7bGKETtoA?si=RmY_yDG24EX6i38I)
- [Coding Day 21: Refactoring the Hasql code](https://youtu.be/a9mPNXbT-qw?si=RTtXe6BXnZSQZzh-)

## Articles

- [Organization of Hasql code in a dedicated library](https://github.com/nikita-volkov/hasql-tutorial1)

# Short Example

Following is a complete application, which performs some arithmetic in Postgres using Hasql.

```haskell
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

import Data.Functor.Contravariant
import Data.Int
import Hasql.Session (Session)
import Hasql.Statement (Statement (..))
import Prelude
import qualified Hasql.Connection as Connection
import qualified Hasql.Connection.Setting as Connection.Setting
import qualified Hasql.Connection.Setting.Connection as Connection.Setting.Connection
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as Session

main :: IO ()
main = do
  Right connection <- Connection.acquire connectionSettings
  result <- Session.run (sumAndDivModSession 3 8 3) connection
  print result
  where
    connectionSettings =
      [ Connection.Setting.connection
          ( Connection.Setting.Connection.params
              [ Connection.Setting.Connection.Param.host "localhost",
                Connection.Setting.Connection.Param.port 5432,
                Connection.Setting.Connection.Param.user "postgres",
                Connection.Setting.Connection.Param.password "postgres",
                Connection.Setting.Connection.Param.dbname "postgres"
              ]
          ),
        -- States that this connection can prepare statements.
        -- True by default.
        Connection.Setting.usePreparedStatements True
      ]

-- * Sessions

-- Session abstracts over the execution of operations on a database connection.
-- It is composable and has a Monad instance.
-------------------------

sumAndDivModSession :: Int64 -> Int64 -> Int64 -> Session (Int64, Int64)
sumAndDivModSession a b c = do
  -- Get the sum of a and b
  sumOfAAndB <- Session.statement (a, b) sumStatement
  -- Divide the sum by c and get the modulo as well
  Session.statement (sumOfAAndB, c) divModStatement

-- * Statements

-- Statement is a definition of an individual SQL-statement,
-- accompanied by a specification of how to encode its parameters and
-- decode its result.
-------------------------

-- | A statement with two integer parameters and an integer result.
sumStatement :: Statement (Int64, Int64) Int64
sumStatement = Statement sql encoder decoder preparable
  where
    -- The SQL of the statement, with $1, $2, ... placeholders for parameters.
    sql =
      "select $1 + $2"
    -- Specification of how to encode the parameters of the statement
    -- where the association with placeholders is achieved by order.
    encoder =
      mconcat
        [ -- Encoder of the first parameter as a non-nullable int8.
          -- It extracts the first element of the tuple using the contravariant functor
          -- instance.
          fst >$< Encoders.param (Encoders.nonNullable Encoders.int8),
          -- Encoder of the second parameter,
          -- which extracts the second element of the tuple.
          snd >$< Encoders.param (Encoders.nonNullable Encoders.int8)
        ]
    -- Specification of how to decode the result of the statement.
    -- States that we expect a single row with a single non-nullable int8 column.
    decoder =
      Decoders.singleRow
        (Decoders.column (Decoders.nonNullable Decoders.int8))
    -- States that this statement is allowed to be prepared on the server side.
    -- Unless your application generates the statements dynamically,
    -- you should always set this to True.
    -- If in the connection settings the usePreparedStatements option is set to False,
    -- the statement will not be prepared regardless of this flag.
    preparable = True

divModStatement :: Statement (Int64, Int64) (Int64, Int64)
divModStatement = Statement sql encoder decoder preparable
  where
    sql =
      "select $1 / $2, $1 % $2"
    encoder =
      mconcat
        [ fst >$< Encoders.param (Encoders.nonNullable Encoders.int8),
          snd >$< Encoders.param (Encoders.nonNullable Encoders.int8)
        ]
    -- Decoder that expects a single row with two non-nullable int8 columns,
    -- returning the result as a tuple.
    -- Uses the applicative functor instance to combine two column decoders.
    decoder =
      Decoders.singleRow
        ( (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.int8)
            <*> Decoders.column (Decoders.nonNullable Decoders.int8)
        )
    preparable = True
```

For the general use-case it is advised to prefer declaring statements using the "hasql-th" library, which validates the statements at compile-time and generates codecs automatically. So the above two statements could be implemented the following way:

```haskell
import qualified Hasql.TH as TH -- from "hasql-th"

sumStatement :: Statement (Int64, Int64) Int64
sumStatement =
  [TH.singletonStatement|
    select ($1 :: int8 + $2 :: int8) :: int8
  |]

divModStatement :: Statement (Int64, Int64) (Int64, Int64)
divModStatement =
  [TH.singletonStatement|
    select
      (($1 :: int8) / ($2 :: int8)) :: int8,
      (($1 :: int8) % ($2 :: int8)) :: int8
  |]
```
