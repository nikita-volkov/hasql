module ConnectionString.Types where

import ConnectionString.Types.Gens qualified as Gens
import Data.Map.Strict qualified as Map
import Platform.Prelude
import Test.QuickCheck qualified as QuickCheck

-- |
-- https://www.postgresql.org/docs/17/libpq-connect.html#LIBPQ-CONNSTRING-URIS
--
-- Examples:
--
-- > postgresql://
-- > postgresql://localhost
-- > postgresql://localhost:5433
-- > postgresql://localhost/mydb
-- > postgresql://user@localhost
-- > postgresql://user:secret@localhost
-- > postgresql://other@localhost/otherdb?connect_timeout=10&application_name=myapp
-- > postgresql://host1:123,host2:456/somedb?target_session_attrs=any&application_name=myapp
data ConnectionString
  = ConnectionString
      -- | User.
      (Maybe Text)
      -- | Password.
      (Maybe Text)
      -- | Host specification.
      [Host]
      -- | Database name.
      (Maybe Text)
      -- | Key-value parameters.
      (Map.Map Text Text)
  deriving (Eq)

data Host
  = Host
      -- | Host domain name or IP-address.
      Text
      -- | Port number.
      (Maybe Word16)
  deriving (Show, Eq)

instance Semigroup ConnectionString where
  ConnectionString user1 password1 hosts1 dbname1 params1 <> ConnectionString user2 password2 hosts2 dbname2 params2 =
    ConnectionString
      (user1 <|> user2)
      (password1 <|> password2)
      (hosts1 <> hosts2)
      (dbname1 <|> dbname2)
      (Map.union params2 params1)

instance Monoid ConnectionString where
  mempty = ConnectionString Nothing Nothing [] Nothing Map.empty

instance QuickCheck.Arbitrary ConnectionString where
  arbitrary = QuickCheck.sized \size -> do
    user <- Gens.genMaybeText size
    -- Password only makes sense if there's a user
    password <- case user of
      Nothing -> pure Nothing
      Just _ -> Gens.genMaybeText size
    hosts <- QuickCheck.scale (`div` 2) (QuickCheck.listOf QuickCheck.arbitrary)
    dbname <- Gens.genMaybeText size
    params <- Gens.genParams size
    pure (ConnectionString user password hosts dbname params)

instance QuickCheck.Arbitrary Host where
  arbitrary = do
    hostname <- Gens.genHostname
    port <- QuickCheck.oneof [pure Nothing, Just <$> QuickCheck.arbitrary]
    pure (Host hostname port)
