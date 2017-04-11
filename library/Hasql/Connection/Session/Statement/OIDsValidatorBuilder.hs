module Hasql.Connection.Session.Statement.OIDsValidatorBuilder
where

import Hasql.Prelude
import qualified VectorBuilder.Builder as N
import qualified VectorBuilder.Vector as O
import qualified Data.Vector as P


type Builder =
  N.Builder (Word32 -> Maybe Text)

build :: Builder -> Vector Word32 -> Maybe Text
build builder =
  let
    !validators =
      O.build builder
    !validatorsLength =
      P.length validators
    in
      \oids ->
      let
        oidsLength =
          P.length oids
        in if oidsLength /= validatorsLength
          then
            Just $
            "Incorrect amount of columns being parsed. Expected: " <>
            (fromString . show) validatorsLength <> ", actual: " <>
            (fromString . show) oidsLength
          else
            getOption $ flip foldMap [0..pred oidsLength] $ \index ->
            let
              validator =
                P.unsafeIndex validators index
              oid =
                P.unsafeIndex oids index
              validation =
                validator oid
              in Option (fmap (mappend "Column " . mappend ((fromString . show) index) . mappend ": ") validation)

validator :: (Word32 -> Maybe Text) -> Builder
validator =
  N.singleton
