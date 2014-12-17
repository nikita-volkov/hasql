import BasePrelude
import MTLPrelude
import Test.Hspec
import Data.Text (Text)
import qualified Hasql as H
import qualified Hasql.Postgres as HP
import qualified SlaveThread


main = 
  hspec $ do

    context "Bug" $ do

      context "Unhandled transaction conflict" $ do

        it "should not be" $ do
          session $ H.tx Nothing $ do
            H.unit [H.q|DROP TABLE IF EXISTS artist|]
            H.unit [H.q|DROP TABLE IF EXISTS artist_union|]
            H.unit $
              [H.q|
                CREATE TABLE "artist_union" (
                  "id" BIGSERIAL,
                  PRIMARY KEY ("id")
                )
              |]
            H.unit $
              [H.q|
                CREATE TABLE "artist" (
                  "id" BIGSERIAL,
                  "artist_union_id" INT8 NOT NULL,
                  "names" TEXT[] NOT NULL,
                  PRIMARY KEY ("id"),
                  FOREIGN KEY ("artist_union_id") REFERENCES "artist_union" ("id") ON DELETE CASCADE
                )
              |]
          (signal, block) <- newBatchGate 6
          let 
            insertArtistUnion :: H.Tx HP.Postgres s Int64
            insertArtistUnion =
              fmap (runIdentity . fromJust) $ H.single $
              [H.q| 
                INSERT INTO artist_union DEFAULT VALUES RETURNING id
              |]
            insertArtist :: Int64 -> [Text] -> H.Tx HP.Postgres s Int64
            insertArtist unionID artistNames = 
              fmap (runIdentity . fromJust) $ H.single $
              [H.q| 
                INSERT INTO artist
                  (artist_union_id, 
                   names)
                  VALUES (?, ?)
                  RETURNING id 
              |]
                unionID
                artistNames
            process = 
                SlaveThread.fork $ do
                  session $ replicateM_ 100 $ do 
                    H.tx (Just (H.Serializable, True)) $ do
                      unionID <- insertArtistUnion
                      insertArtist unionID ["a", "b", "c"]
                  signal
          replicateM_ 6 process
          block

    context "RowParser" $ do

      it "should fail on incorrect arity" $ do
        flip shouldThrow (\case H.UnparsableRow _ -> True; _ -> False) $
          session $ do
            H.tx Nothing $ do
              H.unit [H.q|DROP TABLE IF EXISTS data|]
              H.unit [H.q|CREATE TABLE data (
                              field1    DECIMAL NOT NULL,
                              field2    BIGINT  NOT NULL,
                              PRIMARY KEY (field1)
                          )|]
              H.unit [H.q|INSERT INTO data (field1, field2) VALUES (0, 0)|]
            mrow :: Maybe (Double, Int64, String) <- 
              H.tx Nothing $  
                H.single $ [H.q|SELECT * FROM data|]
            return ()


-- * Helpers
-------------------------

session :: (forall s. H.Session HP.Postgres s IO r) -> IO r
session =
  H.session backendSettings poolSettings
  where
    backendSettings = HP.ParamSettings "localhost" 5432 "postgres" "" "postgres"
    poolSettings = fromJust $ H.sessionSettings 6 30

newBatchGate :: Int -> IO (IO (), IO ())
newBatchGate amount =
  do
    counter <- atomically $ newTVar amount
    return $
      let signal = atomically $ readTVar counter >>= writeTVar counter . pred
          block = atomically $ readTVar counter >>= \x -> when (x > 0) retry
          in (signal, block)
