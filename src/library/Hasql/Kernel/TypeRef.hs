module Hasql.Kernel.TypeRef
  ( TypeRef (..),
  )
where

import Hasql.Kernel.QualifiedTypeName (QualifiedTypeName)
import Hasql.Platform.Prelude

-- |
-- How a parameter's Postgres type is identified within parameter metadata:
-- either an already-known OID, or a 'QualifiedTypeName' still pending OID
-- resolution against the server.
data TypeRef
  = -- | The type's OID is statically known.
    KnownOid Word32
  | -- | The type is named and its OID must be resolved before execution.
    NamedType QualifiedTypeName
  deriving stock (Eq, Ord, Show, Generic)

instance Hashable TypeRef
