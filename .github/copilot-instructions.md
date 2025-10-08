# GitHub Copilot Instructions for Hasql

## Project Overview

Hasql is a fast PostgreSQL driver for Haskell with a flexible mapping API. It serves as the root of a granular ecosystem of composable libraries, each designed to perform specific tasks while staying simple. The project emphasizes modularity, type safety, and explicit error handling over exceptions.

## Architecture & Design Philosophy

### Ecosystem Approach
- **Modular Design**: Instead of a monolithic library, hasql follows an ecosystem approach with small, focused libraries
- **Horizontal Scalability**: Users are encouraged to create extension libraries rather than contribute features to the core
- **Composability**: Each library provides a simple API that can be combined with others
- **Interchangeability**: Multiple libraries can solve the same problem with different approaches

### Key Abstractions
- **Connection**: Manages database connections with settings and prepared statement registries
- **Session**: A batch of actions executed in a database connection context (ReaderT + ExceptT)
- **Pipeline**: Composable abstraction for executing multiple queries efficiently
- **Statement**: Specification of a single SQL query with parameter/result mapping
- **Encoders**: DSL for declaring parameter encoders (Params, Value, NullableOrNot)
- **Decoders**: DSL for declaring result decoders (Result, Row, Value, NullableOrNot)

## Code Style & Conventions

### Language Extensions
The project uses modern Haskell with these standard extensions:
- `ApplicativeDo`, `Arrows`, `BangPatterns`, `BlockArguments`
- `ConstraintKinds`, `DataKinds`, `DefaultSignatures`
- `DeriveFoldable`, `DeriveFunctor`, `DeriveGeneric`
- `DerivingVia`, `DuplicateRecordFields`, `FlexibleContexts`
- `FlexibleInstances`, `FunctionalDependencies`, `GADTs`
- `GeneralizedNewtypeDeriving`, `LambdaCase`, `LiberalTypeSynonyms`
- `MultiParamTypeClasses`, `NoImplicitPrelude`, `NoMonomorphismRestriction`
- `OverloadedStrings`, `QuasiQuotes`, `RankNTypes`, `RecordWildCards`
- `ScopedTypeVariables`, `StrictData`, `TemplateHaskell`, `TupleSections`
- `TypeApplications`, `TypeFamilies`, `TypeOperators`, `UndecidableInstances`

### Import Conventions
- **Qualified Imports**: Extensively used for clarity (e.g., `qualified as Encoders`, `qualified as Decoders`)
- **Custom Prelude**: Uses `Platform.Prelude` instead of standard Prelude

### Naming Patterns
- **Newtype Wrappers**: Extensive use for type safety (Session, Statement, Connection)
- **DSL Style**: Encoders and Decoders use fluent DSL patterns
- **Explicit Types**: Clear, descriptive type signatures with phantom types where necessary

### Error Handling
- **No Exceptions**: Explicit error handling using `Either` types
- **Result Types**: All operations return explicit success/failure types

### Applicative syntax
- Prefer `do` notation with `ApplicativeDo` for clarity in applicative contexts

### Function application and chaining
- Avoid using `$` for function application; prefer parentheses for clarity
- When chaining functions instead of nesting parentheses, use the `.` operator and wrap the chain in parentheses to avoid the Ormolu formatter splitting such chains into multiple lines.
  
  E.g.,
  ```haskell
  (TextBuilder.toText . mconcat)
    [ ... ]
  ```
- Use of the `&` operator is acceptable

### Constructing Text
- **Text Builders**: Use `TextBuilder` from the "text-builder" library for efficient text construction
- **Concatenation**: Prefer `mconcat` over series of `(<>)`
- Use `(TextBuilder.toText . mconcat)` for immediately converting to `Text`

## Code Generation Guidelines

When working with this codebase:

1. **Use qualified imports** for all major modules to maintain clarity
2. **Follow the newtype pattern** for type safety when introducing new abstractions
3. **Maintain the Either-based error handling** - never throw exceptions
4. **Use the custom Prelude** (`Platform.Prelude`) in all modules
5. **Keep modules focused** - each module should have a single, clear responsibility
6. **Follow the existing DSL patterns** when extending encoders/decoders

## Build System

- **Cabal**: Uses Cabal as the build system
- **Multiple Components**: Library, test suites, benchmarks are separate components
- **GHC Requirements**: Requires recent GHC with extensive language extension support

## Extension Libraries

When suggesting new functionality, consider whether it belongs in:
- **hasql-th**: Template Haskell utilities and compile-time checking
- **hasql-transaction**: STM-inspired transaction management
- **hasql-dynamic-statements**: Dynamic statement generation
- **hasql-cursor-query**: Cursor-based query abstractions
- **hasql-implicits**: Implicit definitions and default codecs
- Or a new focused extension library

## Documentation Style

- Use Haddock comments with `-- |` for module and function documentation
- Include practical examples in documentation
- Reference the broader ecosystem when relevant
- Maintain clear separation between core functionality and extensions

## Integration tests

- Split the integration tests into two categories: those that are safe to run with a shared database and those that require isolated test environments. Depending on this place them in either the `Sharing` or `Isolated` namespace.

- Ideally all tests should be modularized by the tested functionality unit. I.e., a specific function. One test module per unit. E.g., tests for the `Hasql.Session.script` function should be located in either `Sharing.ByUnit.Session.ScriptSpec` or `Isolated.ByUnit.Session.ScriptSpec`.
