-- You can execute this file with 'cabal bench demo'.
{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, OverloadedStrings #-}

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Functor.Identity

-- Import the API from the "hasql" library
import qualified Hasql as H

-- Import the backend settings from the "hasql-postgres" library
import qualified Hasql.Postgres as H


main = do

  let postgresSettings = H.Postgres "localhost" 5432 "postgres" "" "postgres"

  -- Prepare session settings with a smart constructor,
  -- which checks the inputted values on correctness.
  -- Set the connections pool size to 6 and the timeout to 30.
  sessionSettings <- maybe (fail "Improper session settings") return $ 
                     H.sessionSettings 6 30

  -- Run a database session,
  -- while automatically managing the resources and exceptions.
  H.session postgresSettings sessionSettings $ do

    -- Execute a group of statements without any locking and ACID guarantees:
    H.tx Nothing $ do
      H.unit [H.q|DROP TABLE IF EXISTS a|]
      H.unit [H.q|CREATE TABLE a (id SERIAL NOT NULL, balance INT8, PRIMARY KEY (id))|]
      -- Insert three rows:
      replicateM_ 3 $ do 
        H.unit [H.q|INSERT INTO a (balance) VALUES (0)|]

    -- Declare a list of transfer settings, which we'll later use.
    -- The tuple structure is: 
    -- @(withdrawalAccountID, arrivalAccountID, amount)@
    let transfers :: [(Int, Int, Int)] = 
          [(1, 2, 20), (2, 1, 30), (2, 3, 100)]

    forM_ transfers $ \(id1, id2, amount) -> do
      -- Run a transaction with ACID guarantees.
      -- Hasql will automatically roll it back and retry it in case of conflicts.
      H.tx (Just (H.Serializable, True)) $ do
        -- Use MaybeT to handle empty results:
        runMaybeT $ do
          do
            -- To distinguish results rows containing just one column, 
            -- we use 'Identity' as a sort of a single element tuple.
            Identity balance1 <- MaybeT $ H.single $ [H.q|SELECT balance FROM a WHERE id=?|] id1
            Identity balance2 <- MaybeT $ H.single $ [H.q|SELECT balance FROM a WHERE id=?|] id2
            lift $ H.unit $ [H.q|UPDATE a SET balance=? WHERE id=?|] (balance1 - amount) id1
            lift $ H.unit $ [H.q|UPDATE a SET balance=? WHERE id=?|] (balance2 + amount) id2

    -- Output all the updated rows:
    do
      -- Unfortunately in this case there's no way to infer the type of the results,
      -- so we need to specify it explicitly:
      rows :: [(Int, Int)] <- H.tx Nothing $ H.list $ [H.q|SELECT * FROM a|]
      forM_ rows $ \(id, amount) -> do
        liftIO $ putStrLn $ "ID: " ++ show id ++ ", Amount: " ++ show amount




