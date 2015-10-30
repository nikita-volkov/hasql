# Hasql [![Build Status](https://travis-ci.org/nikita-volkov/hasql.svg?branch=master)](https://travis-ci.org/nikita-volkov/hasql)


Hasql provides a robust and concise yet powerful API for communication with arbitrary relational databases using SQL. 

Currently the only backend available is for PostgreSQL ([which can yield great performance improvements](https://nikita-volkov.github.io/hasql-benchmarks/) over HDBC or postgresql-simple).

The code used here file is the [demo found in the repository](https://github.com/nikita-volkov/hasql/blob/master/demo/Main.hs)

## Opening a connection

For greater convenience the Hasql has a built-in connection pool. All interactions with the database backend are done within the context of such a pool. 

So we have functions to create a pool and one to release all resources held by the pool:

```haskell
H.acquirePool
  :: Hasql.Backend.Cx c =>
     Hasql.Backend.CxSettings c -> H.PoolSettings -> IO (H.Pool c)
```

and

```haskell
H.releasePool :: H.Pool c -> IO ()
```

To create the pool we need to pass the connection settings (which are backend dependent) and the pool settings. The code sample below will open a connection to a PostgreSQL database.

```haskell
{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, OverloadedStrings #-}

-- Import the API from the "hasql" library
import qualified Hasql as H

-- Import the backend API from the "hasql-postgres" library
import qualified Hasql.Postgres as HP
let postgresSettings = HP.ParamSettings "localhost" 5432 "postgres" "" "postgres"

-- Prepare the pool settings with a smart constructor,
-- which checks the inputted values on correctness.
-- Set the connection pool size to 6 and the timeout to 30 seconds.
poolSettings <- maybe (fail "Improper session settings") return $ 
                  H.poolSettings 6 30

-- Acquire the database connections pool.
-- Gotta help the compiler with the type signature of the pool a bit.
pool :: H.Pool HP.Postgres 
       <- H.acquirePool postgresSettings poolSettings
```

## Executing a statement

To execute statements we will use a ```Session```, which is just a wrapper for the ```ReaderT``` monad.
This allow us to use the pool for all our session sub-computations.

So the ```session``` function, is a wrapper for the ```runReaderT```, and besides a parameter with the pool, 
we need to pass a function with the return type in ```H.Session``` monad. And as the return we get 
either a ```SessionError``` or the result of our function. 

The function we will use to actually execute the transactions is the ```tx```
which conveniently enough receives a transaction mode, the transactions we want to execute (along with their session context) 
and returns the type ```H.Session c m r```.

It is **important** to notice that running ```IO``` in ```Tx``` is prohibited. 

let's take a look at the signatures proceding:

```haskell
H.session
  :: H.Pool c -> H.Session c m a -> m (Either (H.SessionError c) a)

H.tx
  :: (Control.Monad.Trans.Control.MonadBaseControl IO m,
      Hasql.Backend.CxTx c) =>
     H.TxMode -> (forall s. H.Tx c s r) -> H.Session c m r
```

The following code excerpt shows us how the demo code uses these functions to open a 
session and start a transaction to create a new table:

```haskell
-- Provide a context for execution of transactions.
-- 'Session' is merely a convenience wrapper around 'ReaderT'.
H.session pool $ do

  -- Execute a group of statements without any locking and ACID guarantees:
  H.tx Nothing $ do
    H.unitEx [H.stmt|DROP TABLE IF EXISTS a|]
    H.unitEx [H.stmt|CREATE TABLE a (id SERIAL NOT NULL, balance INT8, PRIMARY KEY (id))|]
```

## Transactions (isolation levels and transaction modes)

You have probably noticed that the first parameter of ```tx``` belongs to the type ```TxMode```.
This parameter deserves some consideration, for it will determine the behaviour of our transaction.
Let's take a look at its type definition:

```haskell
type TxMode = Maybe (TxIsolationLevel, TxWriteMode)

data TxIsolationLevel =
  RepeatableReads |
  Serializable    |
  ReadCommitted   |
  ReadUncommitted

type TxWriteMode = Maybe Bool
```

So when the ```mode``` is ```Nothing```, no transaction is explicitly estabilished on the server.
In PostgreSQL's case this means all commands be commited immediatly after execution 
and their isolation level will be *Read Committed*.

If we pass the tuple, the first element will be the transaction isolation level, you can read more about 
[transaction isolation levels on wikipedia](https://en.wikipedia.org/wiki/Isolation_(database_systems)#Isolation_levels).

The second element is the write mode, which will be interpreted as:

 * ```Nothing``` indicates a "read" mode.
 * ```Just True``` indicates a "write" mode.
 * ```Just False``` indicates a "write" mode without committing (can be useful for testing purposes).

