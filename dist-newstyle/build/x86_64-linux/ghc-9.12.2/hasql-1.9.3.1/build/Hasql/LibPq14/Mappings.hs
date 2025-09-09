{-# LINE 1 "library/Hasql/LibPq14/Mappings.hsc" #-}
module Hasql.LibPq14.Mappings where



import Foreign.C.Types (CInt (..))
import Hasql.Prelude

data ExecStatus
  = EmptyQuery
  | CommandOk
  | TuplesOk
  | CopyOut
  | CopyIn
  | CopyBoth
  | BadResponse
  | NonfatalError
  | FatalError
  | SingleTuple
  | PipelineSync
  | PipelineAbort
  deriving (Eq, Show)

decodeExecStatus :: CInt -> Maybe ExecStatus
decodeExecStatus = \case
  (0) -> Just EmptyQuery
{-# LINE 26 "library/Hasql/LibPq14/Mappings.hsc" #-}
  (1) -> Just CommandOk
{-# LINE 27 "library/Hasql/LibPq14/Mappings.hsc" #-}
  (2) -> Just TuplesOk
{-# LINE 28 "library/Hasql/LibPq14/Mappings.hsc" #-}
  (3) -> Just CopyOut
{-# LINE 29 "library/Hasql/LibPq14/Mappings.hsc" #-}
  (4) -> Just CopyIn
{-# LINE 30 "library/Hasql/LibPq14/Mappings.hsc" #-}
  (8) -> Just CopyBoth
{-# LINE 31 "library/Hasql/LibPq14/Mappings.hsc" #-}
  (5) -> Just BadResponse
{-# LINE 32 "library/Hasql/LibPq14/Mappings.hsc" #-}
  (6) -> Just NonfatalError
{-# LINE 33 "library/Hasql/LibPq14/Mappings.hsc" #-}
  (7) -> Just FatalError
{-# LINE 34 "library/Hasql/LibPq14/Mappings.hsc" #-}
  (9) -> Just SingleTuple
{-# LINE 35 "library/Hasql/LibPq14/Mappings.hsc" #-}
  (10) -> Just PipelineSync
{-# LINE 36 "library/Hasql/LibPq14/Mappings.hsc" #-}
  (11) -> Just PipelineAbort
{-# LINE 37 "library/Hasql/LibPq14/Mappings.hsc" #-}
  _ -> Nothing

encodeExecStatus :: ExecStatus -> CInt
encodeExecStatus = \case
  EmptyQuery -> 0
{-# LINE 42 "library/Hasql/LibPq14/Mappings.hsc" #-}
  CommandOk -> 1
{-# LINE 43 "library/Hasql/LibPq14/Mappings.hsc" #-}
  TuplesOk -> 2
{-# LINE 44 "library/Hasql/LibPq14/Mappings.hsc" #-}
  CopyOut -> 3
{-# LINE 45 "library/Hasql/LibPq14/Mappings.hsc" #-}
  CopyIn -> 4
{-# LINE 46 "library/Hasql/LibPq14/Mappings.hsc" #-}
  CopyBoth -> 8
{-# LINE 47 "library/Hasql/LibPq14/Mappings.hsc" #-}
  BadResponse -> 5
{-# LINE 48 "library/Hasql/LibPq14/Mappings.hsc" #-}
  NonfatalError -> 6
{-# LINE 49 "library/Hasql/LibPq14/Mappings.hsc" #-}
  FatalError -> 7
{-# LINE 50 "library/Hasql/LibPq14/Mappings.hsc" #-}
  SingleTuple -> 9
{-# LINE 51 "library/Hasql/LibPq14/Mappings.hsc" #-}
  PipelineSync -> 10
{-# LINE 52 "library/Hasql/LibPq14/Mappings.hsc" #-}
  PipelineAbort -> 11
{-# LINE 53 "library/Hasql/LibPq14/Mappings.hsc" #-}

data PipelineStatus
  = PipelineOn
  | PipelineOff
  | PipelineAborted
  deriving (Eq, Show)

decodePipelineStatus :: CInt -> Maybe PipelineStatus
decodePipelineStatus = \case
  (1) -> Just PipelineOn
{-# LINE 63 "library/Hasql/LibPq14/Mappings.hsc" #-}
  (0) -> Just PipelineOff
{-# LINE 64 "library/Hasql/LibPq14/Mappings.hsc" #-}
  (2) -> Just PipelineAborted
{-# LINE 65 "library/Hasql/LibPq14/Mappings.hsc" #-}
  _ -> Nothing

decodeBool :: CInt -> Maybe Bool
decodeBool = \case
  0 -> Just False
  1 -> Just True
  _ -> Nothing
