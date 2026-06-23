module Hasql.Codecs.Vocab.QualifiedTypeName
  ( QualifiedTypeName (..),
    fromNameTuple,
    toNameTuple,
  )
where

import Hasql.Platform.Prelude

-- |
-- A Postgres type identified by name: an optional schema together with a
-- required type name.
--
-- A 'Nothing' schema means the name is unqualified and is resolved via the
-- server's search path.
--
-- Used as the key under which a type's OIDs are resolved and cached.
data QualifiedTypeName = QualifiedTypeName
  { schema :: Maybe Text,
    name :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Hashable QualifiedTypeName

-- | An unqualified name constructor for convenience.
instance IsString QualifiedTypeName where
  fromString = QualifiedTypeName Nothing . fromString

-- |
-- Convert from the legacy @(schema, name)@ tuple representation.
--
-- Used at public-API boundaries (e.g. the @custom@ codecs and error types)
-- where the tuple is still exposed but internals operate on 'QualifiedTypeName'.
fromNameTuple :: (Maybe Text, Text) -> QualifiedTypeName
fromNameTuple (schema, name) = QualifiedTypeName schema name

-- |
-- Convert to the legacy @(schema, name)@ tuple representation.
--
-- See 'fromNameTuple'.
toNameTuple :: QualifiedTypeName -> (Maybe Text, Text)
toNameTuple (QualifiedTypeName schema name) = (schema, name)
