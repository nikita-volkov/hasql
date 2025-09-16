module Core.Contexts.OIDLookup where

import Core.Contexts.Command qualified as Command
import Core.Contexts.ResultConsumer qualified as ResultConsumer
import Core.Contexts.RowDecoder qualified as RowDecoder  
import Core.Contexts.ValueDecoder qualified as ValueDecoder
import Core.PostgresTypeInfo qualified as PTI
import Core.Structures.OIDCache qualified as OIDCache
import Platform.Prelude
import PostgreSQL.Binary.Decoding qualified as A
import Pq qualified

-- | Look up type information by name from pg_type
-- For now, this is a placeholder that will be implemented once we have the named codecs working
lookupTypeInfo :: Text -> Command.Command (Maybe OIDCache.TypeInfo)
lookupTypeInfo _typeName = do
  -- TODO: Implement actual pg_type lookup once we have text encoding resolved
  -- This will be completed when we add the named codec functions
  pure Nothing