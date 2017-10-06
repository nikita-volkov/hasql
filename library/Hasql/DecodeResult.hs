module Hasql.DecodeResult where

import Hasql.Prelude
import Hasql.Model
import qualified Hasql.ParseMessageStream as A


newtype DecodeResult result =
  DecodeResult (Bool -> A.ParseMessageStream result)
