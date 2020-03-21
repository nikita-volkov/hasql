module Main.Prelude
(
  module Exports,
  mapLeft,
)
where


-- rerebase
-------------------------
import Prelude as Exports

{-# INLINE mapLeft #-}
mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f =
  either (Left . f) Right
