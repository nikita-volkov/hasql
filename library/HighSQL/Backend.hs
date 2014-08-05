-- |
-- An open API for implementation of specific backend drivers.
module HighSQL.Backend where

import HighSQL.Prelude hiding (Error)
import qualified Data.Text as Text
import qualified Language.Haskell.TH as TH


data Backend =
  Backend {
    connect :: IO Connection
  }


data Connection =
  forall s. 
  Connection {
    -- |
    -- Start a transaction in a write mode if the flag is true.
    beginTransaction :: Bool -> IO (),
    -- |
    -- Finish the transaction, 
    -- while releasing all the resources acquired with 'executeAndStream'.
    -- 
    -- The boolean defines whether to commit the updates,
    -- otherwise it rolls back.
    finishTransaction :: Bool -> IO (),
    -- |
    -- If the backend supports statement preparation,
    -- this function sompiles a bytestring statement 
    -- with placeholders if it's not compiled already,
    -- and otherwise returns the cached statement. 
    -- IOW, implements memoization.
    -- 
    -- If the backend does not support this,
    -- then this function should simply be implemented as a 'return'.
    prepare :: ByteString -> IO s,
    -- |
    -- Execute a statement with values for placeholders.
    execute :: s -> [Value] -> IO Int,
    -- |
    -- Execute a statement with values and an expected results stream size.
    -- The expected stream size can be used by the backend to determine 
    -- an optimal fetching method.
    executeAndStream :: s -> [Value] -> Maybe Int -> IO ResultSet,
    -- |
    -- Close the connection.
    disconnect :: IO ()
  }


data Error =
  -- |
  -- A transaction failed and should be retried.
  TransactionError |
  -- |
  -- Cannot connect to a server 
  -- or the connection got interrupted.
  ConnectionError Text |
  -- |
  -- A free-form backend-specific exception.
  BackendError SomeException
  deriving (Show, Typeable)

instance Exception Error


-- |
-- A row width and a stream of values.
-- The length of the stream must be a multiple of the row width.
type ResultSet =
  (Int, ListT IO Value)


-- * Value
-------------------------

data Value =
  Value_Text !Text |
  Value_ByteString !ByteString |
  Value_Word32 !Word32 |
  Value_Word64 !Word64 |
  Value_Int32 !Int32 |
  Value_Int64 !Int64 |
  Value_Integer !Integer |
  Value_Char !Char |
  Value_Bool !Bool |
  Value_Double !Double |
  Value_Rational !Rational |
  Value_Day !Day |
  Value_LocalTime !LocalTime |
  Value_TimeOfDay !TimeOfDay |
  Value_ZonedTime !ZonedTime |
  Value_UTCTime !UTCTime |
  Value_NominalDiffTime !NominalDiffTime |
  Value_Null
  deriving (Show, Data, Typeable, Generic)


class ValueConversion a where
  toValue :: a -> Value
  fromValue :: Value -> Maybe a

-- Generate standard instances using Template Haskell:
$(
  let 
    assocs =
      [
        (''NominalDiffTime, 'Value_NominalDiffTime),
        (''UTCTime, 'Value_UTCTime),
        (''ZonedTime, 'Value_ZonedTime),
        (''TimeOfDay, 'Value_TimeOfDay),
        (''LocalTime, 'Value_LocalTime),
        (''Day, 'Value_Day),
        (''Rational, 'Value_Rational),
        (''Double, 'Value_Double),
        (''Bool, 'Value_Bool),
        (''Char, 'Value_Char),
        (''Integer, 'Value_Integer),
        (''Int64, 'Value_Int64),
        (''Int32, 'Value_Int32),
        (''Word64, 'Value_Word64),
        (''Word32, 'Value_Word32),
        (''ByteString, 'Value_ByteString),
        (''Text, 'Value_Text)
      ]
    inst :: TH.Name -> TH.Name -> TH.Dec
    inst t c = 
      TH.InstanceD [] (TH.AppT (TH.ConT ''ValueConversion) (TH.ConT t)) [d1, d2]
      where
        d1 =
          TH.FunD 'toValue [TH.Clause [] (TH.NormalB (TH.ConE c)) []]
        d2 = 
          TH.FunD 'fromValue [c1, c2]
          where
            c1 = 
              TH.Clause [p] (TH.NormalB e) []
              where
                p = TH.ConP c [TH.VarP v]
                v = TH.mkName "a"
                e = TH.AppE (TH.ConE 'Just) (TH.VarE v)
            c2 = 
              TH.Clause [TH.WildP] (TH.NormalB (TH.ConE 'Nothing)) []
    in return $ map (uncurry inst) assocs
  )

instance ValueConversion String where
  toValue = Value_Text . Text.pack
  fromValue = \case Value_Text a -> Just (Text.unpack a); _ -> Nothing

instance ValueConversion Word where
  toValue = Value_Word64 . fromIntegral
  fromValue = \case Value_Word64 a -> Just (fromIntegral a); _ -> Nothing

instance ValueConversion Int where
  toValue = Value_Int64 . fromIntegral
  fromValue = \case Value_Int64 a -> Just (fromIntegral a); _ -> Nothing
