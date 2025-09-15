module Hasql.Connection.PqProcedures.ServerVersion
  ( ServerVersion (..),
    fromInt,
    toText,
  )
where

import Platform.Prelude
import TextBuilder qualified

data ServerVersion = ServerVersion Int Int Int
  deriving stock (Eq, Ord, Show)

-- |
-- >>> fromInt 10000
-- ServerVersion 1 0 0
--
-- >>> fromInt 100001
-- ServerVersion 10 1 0
--
-- >>> fromInt 110000
-- ServerVersion 11 0 0
--
-- >>> fromInt 90105
-- ServerVersion 9 1 5
--
-- >>> fromInt 90200
-- ServerVersion 9 2 0
--
-- Ref: https://www.postgresql.org/docs/17/libpq-status.html#LIBPQ-PQSERVERVERSION
fromInt :: Int -> ServerVersion
fromInt x =
  if x < 100_000
    then fromPre10Int x
    else fromPost10Int x

fromPost10Int :: Int -> ServerVersion
fromPost10Int x =
  let (major, minor) = divMod x 10_000
   in ServerVersion major minor 0

fromPre10Int :: Int -> ServerVersion
fromPre10Int = evalState do
  patch <- state (swap . flip divMod 100)
  minor <- state (swap . flip divMod 100)
  major <- get
  pure (ServerVersion major minor patch)

toText :: ServerVersion -> Text
toText (ServerVersion major minor patch) =
  (TextBuilder.toText . mconcat)
    [ TextBuilder.decimal major,
      ".",
      TextBuilder.decimal minor,
      ".",
      TextBuilder.decimal patch
    ]
