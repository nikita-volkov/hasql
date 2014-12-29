-- You can execute this file with 'cabal bench demo'.
{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, OverloadedStrings #-}

import Control.Monad hiding (forM_, mapM_, forM, mapM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Functor.Identity
import Data.Foldable

-- Import the API from the "hasql" library
import qualified Hasql as H

-- Import the backend API from the "hasql-postgres" library
import qualified Hasql.Postgres as HP


main = do

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

  -- Provide a context for execution of transactions.
  -- 'Session' is merely a convenience wrapper around 'ReaderT'.
  H.session pool $ do

    -- Execute a group of statements without any locking and ACID guarantees:
    H.tx Nothing $ do
      H.unitEx [H.stmt|DROP TABLE IF EXISTS a|]
      H.unitEx [H.stmt|CREATE TABLE a (id SERIAL NOT NULL, balance INT8, PRIMARY KEY (id))|]
      -- Insert three rows:
      replicateM_ 3 $ do 
        H.unitEx [H.stmt|INSERT INTO a (balance) VALUES (0)|]

    -- Declare a list of transfer settings, which we'll later use.
    -- The tuple structure is: 
    -- @(withdrawalAccountID, arrivalAccountID, amount)@
    let transfers :: [(Int, Int, Int)] = 
          [(1, 2, 20), (2, 1, 30), (2, 3, 100)]

    forM_ transfers $ \(id1, id2, amount) -> do
      -- Run a transaction with ACID guarantees.
      -- Hasql will automatically roll it back and retry it in case of conflicts.
      H.tx (Just (H.Serializable, (Just True))) $ do
        -- Use MaybeT to handle empty results:
        runMaybeT $ do
          -- To distinguish results rows containing just one column, 
          -- we use 'Identity' as a sort of a single element tuple.
          Identity balance1 <- MaybeT $ H.maybeEx $ [H.stmt|SELECT balance FROM a WHERE id=?|] id1
          Identity balance2 <- MaybeT $ H.maybeEx $ [H.stmt|SELECT balance FROM a WHERE id=?|] id2
          lift $ H.unitEx $ [H.stmt|UPDATE a SET balance=? WHERE id=?|] (balance1 - amount) id1
          lift $ H.unitEx $ [H.stmt|UPDATE a SET balance=? WHERE id=?|] (balance2 + amount) id2

    -- Output all the updated rows:
    do
      -- Unfortunately in this case there's no way to infer the type of the results,
      -- so we need to specify it explicitly:
      rows <- H.tx Nothing $ H.vectorEx $ [H.stmt|SELECT * FROM a|]
      forM_ rows $ \(id :: Int, amount :: Int) -> do
        liftIO $ putStrLn $ "ID: " ++ show id ++ ", Amount: " ++ show amount

  -- Release all previously acquired resources. Just for fun.
  H.releasePool pool




