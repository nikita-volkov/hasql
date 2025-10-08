module Helpers.Dsls.Execution
  ( Session,
    sessionByParams,
    Pipeline,
    pipelineByParams,
    generateVarname,
  )
where

import Hasql.Pipeline (Pipeline)
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session (Session)
import Hasql.Session qualified as Session
import Helpers.Dsls.Statement qualified as StatementDsl
import System.Random.Stateful qualified as Random
import TextBuilder qualified
import Prelude

sessionByParams ::
  (StatementDsl.StatementModule params result) =>
  params -> Session result
sessionByParams = Session.pipeline . pipelineByParams

pipelineByParams ::
  (StatementDsl.StatementModule params result) =>
  params -> Pipeline result
pipelineByParams params = Pipeline.statement params StatementDsl.statement

generateVarname :: IO Text
generateVarname = do
  uniqueNum1 <- Random.uniformWord64 Random.globalStdGen
  uniqueNum2 <- Random.uniformWord64 Random.globalStdGen
  pure
    $ TextBuilder.toText
    $ mconcat
    $ [ "testing.v",
        TextBuilder.decimal uniqueNum1,
        "v",
        TextBuilder.decimal uniqueNum2
      ]
