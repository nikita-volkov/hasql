module Platform.Prelude.PlainText where

import Data.Int
import Data.Text qualified
import Data.Text.Lazy qualified
import Data.Word
import Numeric.Natural
import TextBuilder qualified
import Prelude

class ToPlainText a where
  toPlainText :: a -> TextBuilder.TextBuilder

instance ToPlainText String where
  toPlainText = TextBuilder.string

instance ToPlainText Data.Text.Text where
  toPlainText = TextBuilder.text

instance ToPlainText Data.Text.Lazy.Text where
  toPlainText = TextBuilder.lazyText

instance ToPlainText Int where
  toPlainText = TextBuilder.decimal

instance ToPlainText Int8 where
  toPlainText = TextBuilder.decimal

instance ToPlainText Int16 where
  toPlainText = TextBuilder.decimal

instance ToPlainText Int32 where
  toPlainText = TextBuilder.decimal

instance ToPlainText Int64 where
  toPlainText = TextBuilder.decimal

instance ToPlainText Word where
  toPlainText = TextBuilder.decimal

instance ToPlainText Word8 where
  toPlainText = TextBuilder.decimal

instance ToPlainText Word16 where
  toPlainText = TextBuilder.decimal

instance ToPlainText Word32 where
  toPlainText = TextBuilder.decimal

instance ToPlainText Word64 where
  toPlainText = TextBuilder.decimal

instance ToPlainText Integer where
  toPlainText = TextBuilder.decimal

instance ToPlainText Natural where
  toPlainText = TextBuilder.decimal
