module Main.Queries where

import Main.Prelude hiding (def)
import qualified Hasql.Query as HQ
import qualified Hasql.Encoding as HE
import qualified Hasql.Decoding as HD
import qualified Main.Prelude as Prelude


def :: ByteString -> HQ.Query () ()
def sql =
  HQ.Query sql Prelude.def Prelude.def False

plain :: ByteString -> HQ.Query () ()
plain sql =
  HQ.Query sql mempty HD.unit False

dropType :: ByteString -> HQ.Query () ()
dropType name =
  plain $
    "drop type if exists " <> name

createEnum :: ByteString -> [ByteString] -> HQ.Query () ()
createEnum name values =
  plain $
    "create type " <> name <> " as enum (" <> 
    mconcat (intersperse ", " (map (\x -> "'" <> x <> "'") values)) <> ")"

selectList :: HQ.Query () ([] (Int64, Int64))
selectList =
  HQ.Query sql mempty decoder True
  where
    sql =
      "values (1,2), (3,4), (5,6)"
    decoder =
      HD.rowsList ((,) <$> HD.value HD.int8 <*> HD.value HD.int8)
