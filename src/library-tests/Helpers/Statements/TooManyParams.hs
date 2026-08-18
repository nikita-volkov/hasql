module Helpers.Statements.TooManyParams where

import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Statement qualified as Statement
import Helpers.Dsls.Statement
import Prelude

-- | A statement with more parameters than libpq accepts in a single command.
--
-- The wire protocol counts parameters in an @int16@, so a list longer than
-- 65535 is rejected by the client before anything reaches the socket.
--
-- That makes it the reproduction trigger of
-- <https://github.com/nikita-volkov/hasql/issues/326>: placed in the middle
-- of a pipeline it makes the batched send of the whole pipeline fail
-- halfway through, with the commands preceding it already dispatched to the
-- server and the ones following it never sent.
--
-- Kept unpreparable so that the failing send is the statement's own
-- execution rather than a @PARSE@ preceding it, and so that the spec reads
-- the same on preparable and unpreparable connections.
data TooManyParams = TooManyParams

type TooManyParamsResult = ()

instance StatementModule TooManyParams TooManyParamsResult where
  statement =
    Statement.unpreparable
      "select 1"
      (foldMap (const param) [1 .. paramCount])
      Decoders.noResult
    where
      -- One more than the maximum amount of parameters libpq accepts.
      paramCount = 65536 :: Int

      param = contramap (const 1) (Encoders.param (Encoders.nonNullable Encoders.int8))
