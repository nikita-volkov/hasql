module Main.Statements where

import Main.Prelude hiding (def)
import qualified Hasql.Statement as HQ
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import qualified Main.Prelude as Prelude


def :: ByteString -> HQ.Statement () ()
def sql =
  HQ.Statement sql Prelude.def Prelude.def False

plain :: ByteString -> HQ.Statement () ()
plain sql =
  HQ.Statement sql mempty HD.unit False

dropType :: ByteString -> HQ.Statement () ()
dropType name =
  plain $
    "drop type if exists " <> name

createEnum :: ByteString -> [ByteString] -> HQ.Statement () ()
createEnum name values =
  plain $
    "create type " <> name <> " as enum (" <> 
    mconcat (intersperse ", " (map (\x -> "'" <> x <> "'") values)) <> ")"

selectList :: HQ.Statement () ([] (Int64, Int64))
selectList =
  HQ.Statement sql mempty decoder True
  where
    sql =
      "values (1,2), (3,4), (5,6)"
    decoder =
      HD.rowList ((,) <$> HD.column HD.int8 <*> HD.column HD.int8)
