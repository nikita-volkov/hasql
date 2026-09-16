# Hasql

[![Hackage](https://img.shields.io/hackage/v/hasql.svg)](https://hackage.haskell.org/package/hasql)
[![Continuous Haddock](https://img.shields.io/badge/haddock-master-blue)](https://nikita-volkov.github.io/hasql/)

PostgreSQL driver for Haskell, that prioritizes:

- **Reliability.** Failures come back as values, and the type says whether the connection survived them. See [Errors](#errors).
- **Flexibility.** Sessions compose, codecs are assembled from parts, and even the transport is a choice you make. See [Ecosystem](#ecosystem).
- **Performance.** Statements are prepared by default and can be pipelined into a single round-trip via `Hasql.Session.pipeline`.

Hasql is production-ready and actively maintained. It's used by many companies and most notably by the [Postgrest](https://github.com/PostgREST/postgrest) project. The API changes at major versions and each major release is supported for at least a year, as described under [Support policy](#support-policy). Every change is recorded in the [changelog](CHANGELOG.md).

Upgrading from 1.x? `Hasql.Connection.acquire` now takes a transport adapter as its first argument, so switching is a one-argument change, and the error types have been reshaped. The [changelog](CHANGELOG.md) lists every break.

## Getting started

Hasql carries no C dependency. It programs against the ["pqi"](https://github.com/nikita-volkov/pqi) interface, and you pick the adapter that implements it. That means you depend on two packages, not one:

```cabal
build-depends:
  hasql,
  pqi-ffi,  -- or pqi-native
```

`Hasql.Connection.acquire` then takes the adapter explicitly, as its first argument:

```haskell
connection <- Hasql.Connection.acquire Pqi.Ffi.adapter settings
```

["pqi-ffi"](https://github.com/nikita-volkov/pqi-ffi) is the stable, production-proven default. ["pqi-native"](https://github.com/nikita-volkov/pqi-native) is a pure-Haskell alpha with no C dependency at all. [Transport adapters](#transport-adapters) covers how to choose.

## Example

Following is a complete application, which sums three numbers in Postgres by running one statement twice on the same connection.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Functor.Contravariant
import Data.Int
import Hasql.Session (Session)
import Prelude
import qualified Hasql.Connection as Connection
import qualified Hasql.Connection.Settings as Settings
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement
import qualified Pqi.Ffi

main :: IO ()
main = do
  acquisition <- Connection.acquire Pqi.Ffi.adapter settings
  case acquisition of
    Left err -> fail (show err)
    Right connection -> do
      result <- Connection.use connection (sumSession 3 8 4)
      print result
  where
    settings =
      mconcat
        [ Settings.hostAndPort "localhost" 5432,
          Settings.user "postgres",
          Settings.password "postgres",
          Settings.dbname "postgres"
          -- Prepared statements are enabled by default.
          -- To disable them (e.g., for pgbouncer compatibility):
          -- Settings.noPreparedStatements True
        ]

-- | Session abstracts over the execution of operations on a connection.
-- It has a Monad instance, so statements compose.
sumSession :: Int64 -> Int64 -> Int64 -> Session Int64
sumSession a b c = do
  ab <- Session.statement (a, b) sumStatement
  Session.statement (ab, c) sumStatement

-- | Statement is a definition of an individual SQL-statement, accompanied by
-- a specification of how to encode its parameters and decode its result.
sumStatement :: Statement.Statement (Int64, Int64) Int64
sumStatement = Statement.preparable sql encoder decoder
  where
    sql =
      "select $1 + $2"
    encoder =
      mconcat
        [ fst >$< Encoders.param (Encoders.nonNullable Encoders.int8),
          snd >$< Encoders.param (Encoders.nonNullable Encoders.int8)
        ]
    decoder =
      Decoders.singleRow
        (Decoders.column (Decoders.nonNullable Decoders.int8))
```

[Your First Statement](https://github.com/nikita-volkov/hasql-docs/blob/main/your-first-statement.md) is the same program with the annotations left in, explaining the encoder and decoder vocabulary line by line.

## Errors

Every operation reports its failures as values. There are three error types, in `Hasql.Errors`, and which one you get tells you what state you're left in.

`Hasql.Connection.acquire` returns `AcquireError`, organized by the stage that failed: connecting, checking the server version, or initializing session settings.

`Hasql.Connection.use` returns `UseError`, which splits on the only distinction the caller can act on:

```haskell
result <- Connection.use connection session
case result of
  Right a -> pure a
  -- The session failed, the connection is still live and reusable.
  Left (Errors.SessionUseError err) -> ...
  -- The connection is gone. Hasql has already closed it.
  Left (Errors.ConnectionUseError reason) -> ...
```

A `ConnectionUseError` means the handle is spent. Hasql finished the connection before returning, so every later `use` on it reports the same error and `release` is a no-op. Pools must discard it rather than return it. A `SessionUseError` carries a `SessionError` and leaves the connection untouched.

All three types implement `IsError`, which renders a message and details for logging and exposes the server's SQLSTATE through `toSqlState` where one was reported.

## Documentation

The long-form material lives in [hasql-docs](https://github.com/nikita-volkov/hasql-docs).

- [**Your First Statement**](https://github.com/nikita-volkov/hasql-docs/blob/main/your-first-statement.md) - an annotated walkthrough of a complete Hasql program, explaining the encoder and decoder vocabulary line by line. Start here if the [Example](#example) above went past too fast.

- [**Data-Access Architecture**](https://github.com/nikita-volkov/hasql-docs/blob/main/data-access-architecture.md) - a normative reference for organizing database integration code built on Hasql. It specifies how to layer types, statements, transactions and sessions, where the application domain enters the picture, how the three error channels differ, and what to test at each level. Every rule carries its rationale and derives from the capability differences between Hasql's four constructs.

- [**Why Make It an Ecosystem?**](https://github.com/nikita-volkov/hasql-docs/blob/main/why-an-ecosystem.md) - the rationale for splitting Hasql into many small, separately-versioned libraries instead of one large one.

The architecture reference is written to be consumed directly by coding agents as well as by people. Point an agent at [the raw file](https://raw.githubusercontent.com/nikita-volkov/hasql-docs/main/data-access-architecture.md) and it has the whole system in context, with the rules numbered so they can be cited back in review.

Per-module API docs are on [Hackage](https://hackage.haskell.org/package/hasql), and the [continuous Haddock](https://nikita-volkov.github.io/hasql/) tracks `master`.

## Ecosystem

Hasql is not just a single library, it is a granular ecosystem of composable libraries, each isolated to perform its own task and stay simple. Each one is separately versioned and separately owned, so a change in one doesn't ripple into the others and anyone can publish an alternative without asking. Instead of debating how transactions or cursors should be abstracted, the ecosystem carries competing answers side by side. [Why Make It an Ecosystem?](https://github.com/nikita-volkov/hasql-docs/blob/main/why-an-ecosystem.md) gives the full argument.

<!-- TODO: curate this list. Every published extension is still on a pre-2.x
     release, and at least one entry below redirects the reader elsewhere. -->

- ["hasql"](https://github.com/nikita-volkov/hasql) - the root of the ecosystem, which provides the essential abstraction over the PostgreSQL client functionality and mapping of values. Everything else revolves around that library.

- ["hasql-transaction"](https://github.com/nikita-volkov/hasql-transaction) - an STM-inspired composable abstraction over database transactions providing automated conflict resolution.

- ["hasql-pool"](https://github.com/nikita-volkov/hasql-pool) - a Hasql-specialized abstraction over the connection pool.

- ["hasql-postgresql-types"](https://github.com/nikita-volkov/hasql-postgresql-types) - integration with the ["postgresql-types"](https://github.com/nikita-volkov/postgresql-types) library, which is a collection of Haskell types precisely modeling PostgreSQL types without data loss or compromise.

- ["hasql-dynamic-statements"](https://github.com/nikita-volkov/hasql-dynamic-statements) - a toolkit for generating statements based on the parameters.

- ["hasql-th"](https://github.com/nikita-volkov/hasql-th) - Template Haskell utilities, providing compile-time syntax checking and easy statement declaration.

- ["hasql-cursor-query"](https://github.com/nikita-volkov/hasql-cursor-query) - a declarative abstraction over cursors.

- ["hasql-cursor-transaction"](https://github.com/nikita-volkov/hasql-cursor-transaction) - a lower-level abstraction over cursors, which however allows to fetch from multiple cursors simultaneously. Generally though "hasql-cursor-query" is the recommended alternative.

- ["hasql-migration"](https://github.com/tvh/hasql-migration) - A port of postgresql-simple-migration for use with hasql.

- ["hasql-listen-notify"](https://github.com/awkward-squad/hasql-listen-notify) / ["hasql-notifications"](https://github.com/diogob/hasql-notifications) - Support for PostgreSQL asynchronous notifications.

- ["hasql-optparse-applicative"](https://github.com/sannsyn/hasql-optparse-applicative) - "optparse-applicative" parsers for Hasql.

- ["hasql-implicits"](https://github.com/nikita-volkov/hasql-implicits) - implicit definitions, such as default codecs for standard types.

- ["hasql-interpolate"](https://github.com/awkward-squad/hasql-interpolate) - a QuasiQuoter that supports interpolating Haskell expressions into Hasql queries.

<sup>Want to list your package or correct something here? Make a PR.</sup>

### Transport adapters

Unlike the extension libraries above, which are optional, a transport adapter is mandatory: Hasql needs one to talk to the server at all.

- ["pqi"](https://github.com/nikita-volkov/pqi) - the driver-agnostic interface that Hasql programs against. Pulled in automatically. You don't depend on it directly.

- ["pqi-ffi"](https://github.com/nikita-volkov/pqi-ffi) - the stable adapter, backed by the C "libpq" library. It requires "libpq" of at least version 14 to be installed to compile, which typically just means having a recent PostgreSQL distro installed. Through it Hasql is tested against a wide range of PostgreSQL servers, starting from version 9.

- ["pqi-native"](https://github.com/nikita-volkov/pqi-native) - a from-scratch, pure-Haskell implementation of the Postgres wire protocol, with no C dependency at all.

"pqi-native" is thoroughly tested: ["pqi-conformance"](https://github.com/nikita-volkov/pqi-conformance) runs it side by side with "libpq" on the same inputs and checks that the results agree, and the test-suites of "hasql", ["hasql-pool"](https://github.com/nikita-volkov/hasql-pool) and ["hasql-transaction"](https://github.com/nikita-volkov/hasql-transaction) now run against both adapters, so the whole stack above the transport is exercised on it too. It's still labelled **alpha**, because it is not yet proven at production scale. That status lifts when it gets traction and successful usage reports.

The two adapters are fully interchangeable. Swapping between them is a one-argument change, nothing else, so you can try "pqi-native" today with no lock-in and fall back without a rewrite.

## Discussions

Join [GitHub Discussions](https://github.com/nikita-volkov/hasql/discussions) to ask questions, provide feedback, suggest and vote on features, and help shape the future of Hasql.

## Support policy

This policy is intended to balance stability for users with the ability to evolve the library.

Each major release of Hasql is supported for at least **one year** from the date of its first release. During this period, fixes are backported to the latest minor version of that major release.

After the support period ends, the release may continue to work but is no longer guaranteed to receive fixes.

You're welcome to post requests to change the policy or issues if you believe something is not being addressed.
