import BasePrelude
import MTLPrelude
import Control.Monad.Trans.Either
import Control.Monad.Trans.Control
import Test.Hspec
import Data.Text (Text)
import qualified Hasql as H
import qualified Hasql.Postgres as HP
import qualified SlaveThread


main = 
  hspec $ do

    context "UTF-8 templates" $ do

      it "encode properly" $ do
        flip shouldBe (Right (Just (Identity ("Ёжик" :: Text)))) =<< do
          session $ H.tx Nothing $ H.maybeTx $ [H.stmt| SELECT 'Ёжик' |]

    context "Bug" $ do

      context "Unhandled transaction conflict" $ do

        it "should not be" $ do
          session $ H.tx Nothing $ do
            H.unitTx [H.stmt|DROP TABLE IF EXISTS artist|]
            H.unitTx [H.stmt|DROP TABLE IF EXISTS artist_union|]
            H.unitTx $
              [H.stmt|
                CREATE TABLE "artist_union" (
                  "id" BIGSERIAL,
                  PRIMARY KEY ("id")
                )
              |]
            H.unitTx $
              [H.stmt|
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
              fmap (runIdentity . fromJust) $ H.maybeTx $
              [H.stmt| 
                INSERT INTO artist_union DEFAULT VALUES RETURNING id
              |]
            insertArtist :: Int64 -> [Text] -> H.Tx HP.Postgres s Int64
            insertArtist unionID artistNames = 
              fmap (runIdentity . fromJust) $ H.maybeTx $
              [H.stmt| 
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
                    H.tx (Just (H.Serializable, Just True)) $ do
                      unionID <- insertArtistUnion
                      insertArtist unionID ["a", "b", "c"]
                  signal
          replicateM_ 6 process
          block

    context "RowParser" $ do

      it "should fail on incorrect arity" $ do
        flip shouldSatisfy (\case Left (H.UnparsableResult _) -> True; _ -> False) =<< do
          session $ do
            H.tx Nothing $ do
              H.unitTx [H.stmt|DROP TABLE IF EXISTS data|]
              H.unitTx [H.stmt|CREATE TABLE data (
                                   field1    DECIMAL NOT NULL,
                                   field2    BIGINT  NOT NULL,
                                   PRIMARY KEY (field1)
                               )|]
              H.unitTx [H.stmt|INSERT INTO data (field1, field2) VALUES (0, 0)|]
            mrow :: Maybe (Double, Int64, String) <- 
              H.tx Nothing $  
                H.maybeTx $ [H.stmt|SELECT * FROM data|]
            return ()


-- * Helpers
-------------------------

newBatchGate :: Int -> IO (IO (), IO ())
newBatchGate amount =
  do
    counter <- atomically $ newTVar amount
    return $
      let signal = atomically $ readTVar counter >>= writeTVar counter . pred
          block = atomically $ readTVar counter >>= \x -> when (x > 0) retry
          in (signal, block)


-- * Hasql utils
-------------------------

type Session m =
  H.Session HP.Postgres m

session :: MonadBaseControl IO m => Session m r -> m (Either (H.TxError HP.Postgres) r)
session m =
  control $ \unlift -> do
    p <- H.acquirePool backendSettings poolSettings
    r <- unlift $ H.session p m
    H.releasePool p
    return r
  where
    backendSettings = HP.ParamSettings "localhost" 5432 "postgres" "" "postgres"
    poolSettings = fromJust $ H.poolSettings 6 30
