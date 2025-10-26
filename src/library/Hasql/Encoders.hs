-- |
-- A DSL for declaration of statement parameter encoders.
--
-- For compactness of names all the types defined here imply being an encoder.
-- E.g., the `Array` type is an __encoder__ of arrays, not the data-structure itself.
module Hasql.Encoders
  ( module Hasql.Codecs.Encoders,
  )
where

import Hasql.Codecs.Encoders
