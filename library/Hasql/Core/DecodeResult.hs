module Hasql.Core.DecodeResult where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Core.ParseMessageStream as A


newtype DecodeResult result =
  DecodeResult (Bool -> A.ParseMessageStream (Either Text result))
