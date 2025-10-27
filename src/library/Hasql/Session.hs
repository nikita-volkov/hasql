{-# OPTIONS_GHC -Wno-deprecations #-}

module Hasql.Session
  ( Session.Session,
    Session.pipeline,
    Session.script,
    statement,
    Session.onLibpqConnection,
  )
where

import Hasql.Engine.Contexts.Session qualified as Session
import Hasql.Statement qualified as Statement

-- |
-- Execute a statement by providing parameters to it.
statement :: params -> Statement.Statement params result -> Session.Session result
statement params (Statement.Statement sql encoder decoder preparable) =
  Session.statement
    sql
    encoder
    decoder
    preparable
    params
