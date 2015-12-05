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
import qualified Hasql.Query as Query
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders

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
              Query.Query sql mempty Decoders.unit True
              where
                sql =
                  "drop type if exists mood"
            in DSL.query () query
          let
            query =
              Query.Query sql mempty Decoders.unit True
              where
                sql =
                  "create type mood as enum ('sad', 'ok', 'happy')"
            in DSL.query () query
          let
            query =
              Query.Query sql encoder decoder True
              where
                sql =
                  "select ($1 :: mood)"
                decoder =
                  (Decoders.singleRow (Decoders.value (Decoders.enum (Just . id))))
                encoder =
                  Encoders.value (Encoders.enum id)
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
                  Query.Query sql encoder decoder True
                  where
                    sql =
                      "select $1"
                    encoder =
                      Encoders.value Encoders.text
                    decoder =
                      (Decoders.singleRow (Decoders.value (Decoders.text)))
            effect2 =
              DSL.query 1 query
              where
                query =
                  Query.Query sql encoder decoder True
                  where
                    sql =
                      "select $1"
                    encoder =
                      Encoders.value Encoders.int8
                    decoder =
                      (Decoders.singleRow (Decoders.value Decoders.int8))
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
            DSL.query () $ Query.Query sql def decoder False
            where
              sql =
                "delete from a"
              decoder =
                Decoders.rowsAffected
      in actual
    ,
    HUnit.testCase "Result of an auto-incremented column" $
    let
      actualIO =
        DSL.session $ do
          DSL.query () $ Queries.plain $ "drop table if exists a"
          DSL.query () $ Queries.plain $ "create table a (id serial not null, v char not null, primary key (id))"
          id1 <- DSL.query () $ Query.Query "insert into a (v) values ('a') returning id" def (Decoders.singleRow (Decoders.value Decoders.int4)) False
          id2 <- DSL.query () $ Query.Query "insert into a (v) values ('b') returning id" def (Decoders.singleRow (Decoders.value Decoders.int4)) False
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

