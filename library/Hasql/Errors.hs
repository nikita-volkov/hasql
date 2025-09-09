-- | 
-- Re-export of internal errors module for backward compatibility.
-- 
-- This module maintains the public API while the implementation has been moved to Hasql.Internal.Errors.
module Hasql.Errors 
  ( module Hasql.Internal.Errors
  ) where

import Hasql.Internal.Errors