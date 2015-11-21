module Main where

import Main.Prelude hiding (assert, isRight, isLeft)
import Test.QuickCheck.Instances
import Test.Tasty
import qualified Main.Queries as Queries
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.SmallCheck as SmallCheck
import qualified Test.Tasty.QuickCheck as QuickCheck
import qualified Test.QuickCheck as QuickCheck
import qualified Main.DSL as DSL
import qualified Hasql as H
import qualified Hasql.Encoding as HE
import qualified Hasql.Decoding as HD

main =
  defaultMain tree

tree =
  testGroup "All tests"
  [
    HUnit.testCase "Enum" $
    HUnit.assertEqual "" (Right "ok") $
    let
      actual =
        unsafePerformIO $
        DSL.session $ do
          let
            query =
              H.Query sql mempty HD.noResult True
              where
                sql =
                  "drop type if exists mood"
            in DSL.query () query
          let
            query =
              H.Query sql mempty HD.noResult True
              where
                sql =
                  "create type mood as enum ('sad', 'ok', 'happy')"
            in DSL.query () query
          let
            query =
              H.Query sql encoder decoder True
              where
                sql =
                  "select ($1 :: mood)"
                decoder =
                  (HD.singleRow (HD.value (HD.enum (Just . id))))
                encoder =
                  HE.value (HE.enum id)
            in DSL.query "ok" query
      in actual
    ,
    HUnit.testCase "The same prepared statement used on different types" $ 
    HUnit.assertEqual "" (Right ("ok", 1)) $
    let
      actual =
        unsafePerformIO $
        DSL.session $ do
          let
            effect1 =
              DSL.query "ok" query
              where
                query =
                  H.Query sql encoder decoder True
                  where
                    sql =
                      "select $1"
                    encoder =
                      HE.value HE.text
                    decoder =
                      (HD.singleRow (HD.value (HD.text)))
            effect2 =
              DSL.query 1 query
              where
                query =
                  H.Query sql encoder decoder True
                  where
                    sql =
                      "select $1"
                    encoder =
                      HE.value HE.int8
                    decoder =
                      (HD.singleRow (HD.value HD.int8))
            in (,) <$> effect1 <*> effect2
      in actual
    ,
    HUnit.testCase "Affected rows counting" $
    HUnit.assertEqual "" (Right 100) $
    let
      actual =
        unsafePerformIO $
        DSL.session $ do
          dropTable
          createTable
          replicateM_ 100 insertRow
          deleteRows <* dropTable
        where
          dropTable =
            DSL.query () $ Queries.plain $ 
            "drop table if exists a"
          createTable =
            DSL.query () $ Queries.plain $
            "create table a (id bigserial not null, name varchar not null, primary key (id))"
          insertRow =
            DSL.query () $ Queries.plain $
            "insert into a (name) values ('a')"  
          deleteRows =
            DSL.query () $ H.Query sql def decoder False
            where
              sql =
                "delete from a"
              decoder =
                HD.rowsAffected
      in actual
    ,
    HUnit.testCase "Result of an auto-incremented column" $
    let
      actualIO =
        DSL.session $ do
          DSL.query () $ Queries.plain $ "drop table if exists a"
          DSL.query () $ Queries.plain $ "create table a (id serial not null, v char not null, primary key (id))"
          id1 <- DSL.query () $ H.Query "insert into a (v) values ('a') returning id" def (HD.singleRow (HD.value HD.int4)) False
          id2 <- DSL.query () $ H.Query "insert into a (v) values ('b') returning id" def (HD.singleRow (HD.value HD.int4)) False
          DSL.query () $ Queries.plain $ "drop table if exists a"
          pure (id1, id2)
      in HUnit.assertEqual "" (Right (1, 2)) =<< actualIO
    ,
    HUnit.testCase "List decoding" $
    let
      actualIO =
        DSL.session $ DSL.query () $ Queries.selectList
      in HUnit.assertEqual "" (Right [(1, 2), (3, 4), (5, 6)]) =<< actualIO
  ]

