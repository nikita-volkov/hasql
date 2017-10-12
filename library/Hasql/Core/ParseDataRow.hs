module Hasql.Core.ParseDataRow where

import Hasql.Prelude
import qualified BinaryParser as D
import qualified Data.Vector as A


{-|
A specification for processing of DataRow messages.

It is assumed that the size of the input vector is checked externally.
-}
data ParseDataRow result =
  ParseDataRow !Int !(Vector (Maybe ByteString) -> Int -> Either Text result)

deriving instance Functor ParseDataRow

instance Applicative ParseDataRow where
  pure x =
    ParseDataRow 0 (\_ _ -> Right x)
  (<*>) (ParseDataRow leftSize leftInterpreter) (ParseDataRow rightSize rightInterpreter) =
    ParseDataRow
      (leftSize + rightSize)
      (\vec !index -> leftInterpreter vec index <*> rightInterpreter vec (index + leftSize))

nullableColumn :: D.BinaryParser column -> ParseDataRow (Maybe column)
nullableColumn parser =
  ParseDataRow 1 $ \vec index ->
  either (Left . mappend ("Column " <> (fromString . show) index <> ": ")) Right $
  traverse (D.run parser) (A.unsafeIndex vec index)

column :: D.BinaryParser column -> ParseDataRow column
column parser =
  ParseDataRow 1 $ \vec index ->
  either (Left . mappend ("Column " <> (fromString . show) index <> ": ")) Right $
  case A.unsafeIndex vec index of
    Just bytes -> D.run parser bytes
    Nothing -> Left "Unexpected NULL"
