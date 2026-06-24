-- Docs: https://hspec.github.io/hspec-discover.html
module Hasql.Comms.SpecHook where

import Hasql.Platform.Prelude
import Pqi qualified
import Pqi.Ffi qualified as Pqi.Ffi
import Pqi.Native qualified as Pqi.Native
import Test.Hspec
import TestcontainersPostgresql qualified

data ConnectWith where
  ConnectWith :: (Pqi.IsConnection c) => (ByteString -> IO c) -> ConnectWith

type HookedSpec = SpecWith (ConnectWith, Text, Word16)

hook :: HookedSpec -> Spec
hook hookedSpec = parallel do
  byDistro "postgres:9"
  byDistro "postgres:18"
  where
    byDistro tagName =
      describe (toList tagName) do
        aroundAll
          ( TestcontainersPostgresql.run
              TestcontainersPostgresql.Config
                { tagName,
                  auth = TestcontainersPostgresql.CredentialsAuth "postgres" "postgres",
                  forwardLogs = False
                }
          )
          do
            forM_ adapters \(adapterName, cw) ->
              describe adapterName
                $ aroundWith (\action (host, port) -> action (cw, host, port)) (parallel hookedSpec)
    adapters =
      [ ("pqi-ffi", ConnectWith (Pqi.connectdb @Pqi.Ffi.Connection)),
        ("pqi-native", ConnectWith (Pqi.connectdb @Pqi.Native.Connection))
      ]
