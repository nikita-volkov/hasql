# What Hasql is


[![Build Status](https://travis-ci.org/nikita-volkov/hasql.svg?branch=master)](https://travis-ci.org/nikita-volkov/hasql)
[![Hackage](https://img.shields.io/hackage/v/hasql.svg)](https://hackage.haskell.org/package/hasql)

Hasql is a highly efficient PostgreSQL driver and a mapping API. It targets both the users, who need a low level of abstraction, and the users, who face the typical tasks of DB-powered applications, providing them with higher-level APIs.


## Ecosystem

Hasql is not just a single library, it is a granular ecosystem of composable libraries, each isolated to perform its own task and stay simple.

* ["hasql"](https://github.com/nikita-volkov/hasql) - the root of the ecosystem, which provides the essential abstraction over the PostgreSQL client functionality and mapping of values. Everything revolves around that library.

* ["hasql-pool"](https://github.com/nikita-volkov/hasql-pool) - a Hasql-specialized abstraction over the connection pool.

* ["hasql-transaction"](https://github.com/nikita-volkov/hasql-transaction) - an STM-inspired composable abstraction over the database transactions with automated conflict resolution.

* ["hasql-dynamic-statements"](https://github.com/nikita-volkov/hasql-dynamic-statements) - a toolkit for generating statements based on the parameters.

* ["hasql-cursor-query"](https://github.com/nikita-volkov/hasql-cursor-query) - a declarative abstraction over cursors.

* ["hasql-cursor-transaction"](https://github.com/nikita-volkov/hasql-cursor-transaction) - a lower-level abstraction over cursors, which however allows to fetch from multiple cursors simultaneously. Generally though "hasql-cursor-query" is the recommended alternative.

* ["hasql-th"](https://github.com/nikita-volkov/hasql-th) - Template Haskell utilities, such as getting SQL from external files at compile-time. It's planned to extend this library to provide a compile-time checking of the SQL-syntax correctness.

* ["hasql-migration"](https://github.com/tvh/hasql-migration) - A port of postgresql-simple-migration for use with hasql.

* ["hasql-optparse-applicative"](https://github.com/sannsyn/hasql-optparse-applicative) - "optparse-applicative" parsers for Hasql.

### Benefits of being an ecosystem

* **Simplicity.** Each library in isolation provides a simple API, which is hopefully easier to comprehend.

* **Flexibility and composability.** The user picks and chooses the features, thus precisely matching the level of abstraction that he needs for his task.

* **Much more stable and more descriptive semantic versioning.** E.g., a change in the API of the "hasql-transaction" library won't affect any of the other libraries and it gives the user a more precise information about which part of his application he needs to update to conform.

* **Interchangeability and competition of the ecosystem components.** E.g., [not everyone will agree](https://github.com/nikita-volkov/hasql/issues/41) with the restrictive design decisions made in the "hasql-transaction" library. However those decisions are not imposed on the user, and instead of having endless debates about how to abstract over transactions, another extension library can simply be released, which will provide a different interpretation of what the abstraction over transactions should be.

* **Horizontal scalability of the ecosystem.** Instead of posting feature- or pull-requests, the users are encouraged to release their own small extension-libraries, with themselves becoming the copyright owners and taking on the maintenance responsibilities. Compare this model to the classical one, where some core-team is responsible for everything. One is scalable, the other is not.


# Example

Following is a complete application, which performs some arithmetic in Postgres using Hasql.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Prelude
import Data.Int
import Data.Functor.Contravariant
import Hasql.Session (Session)
import Hasql.Statement (Statement(..))
import qualified Hasql.Session as Session
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Connection as Connection


main :: IO ()
main = do
  Right connection <- Connection.acquire connectionSettings
  result <- Session.run (sumAndDivModSession 3 8 3) connection
  print result
  where
    connectionSettings = Connection.settings "localhost" 5432 "postgres" "" "postgres"


-- * Sessions
-- 
-- Session is an abstraction over the database connection and all possible errors.
-- It is used to execute statements.
-- It is composable and has a Monad instance.
-- 
-- It's recommended to define sessions in a dedicated 'Sessions'
-- submodule of your project.
-------------------------

sumAndDivModSession :: Int64 -> Int64 -> Int64 -> Session (Int64, Int64)
sumAndDivModSession a b c = do
  -- Get the sum of a and b
  sumOfAAndB <- Session.statement (a, b) sumStatement
  -- Divide the sum by c and get the modulo as well
  Session.statement (sumOfAAndB, c) divModStatement


-- * Statements
-- 
-- Statement is a definition of an individual SQL-statement,
-- accompanied by a specification of how to encode its parameters and
-- decode its result.
-- 
-- It's recommended to define statements in a dedicated 'Statements'
-- submodule of your project.
-------------------------

sumStatement :: Statement (Int64, Int64) Int64
sumStatement = Statement sql encoder decoder True where
  sql = "select $1 + $2"
  encoder =
    (fst >$< Encoders.param Encoders.int8) <>
    (snd >$< Encoders.param Encoders.int8)
  decoder = Decoders.singleRow (Decoders.column Decoders.int8)

divModStatement :: Statement (Int64, Int64) (Int64, Int64)
divModStatement = Statement sql encoder decoder True where
  sql = "select $1 / $2, $1 % $2"
  encoder =
    (fst >$< Encoders.param Encoders.int8) <>
    (snd >$< Encoders.param Encoders.int8)
  decoder = Decoders.singleRow row where
    row =
      (,) <$>
      Decoders.column Decoders.int8 <*>
      Decoders.column Decoders.int8
```
