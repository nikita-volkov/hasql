module Main where

import Main.Prelude hiding (assert)
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck as QuickCheck
import qualified Main.Queries as Queries
import qualified Main.DSL as DSL
import qualified Main.Connection as Connection
import qualified Hasql.Query as Query
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Session as Session

main =
  defaultMain tree

tree =
  localOption (NumThreads 1) $
  testGroup "All tests"
  [
    testCase "IN simulation" $
    let
      query =
        Query.statement "select true where 1 = any ($1)" encoder decoder True
        where
          encoder =
            Encoders.value (Encoders.array (Encoders.arrayDimension foldl' (Encoders.arrayValue Encoders.int8)))
          decoder =
            fmap (maybe False (const True)) (Decoders.maybeRow (Decoders.value Decoders.bool))
      session =
        do
          result1 <- Session.query [1, 2] query
          result2 <- Session.query [2, 3] query
          return (result1, result2)
      in do
        x <- Connection.with (Session.run session)
        assertEqual (show x) (Right (Right (True, False))) x
    ,
    testCase "NOT IN simulation" $
    let
      query =
        Query.statement "select true where 3 <> all ($1)" encoder decoder True
        where
          encoder =
            Encoders.value (Encoders.array (Encoders.arrayDimension foldl' (Encoders.arrayValue Encoders.int8)))
          decoder =
            fmap (maybe False (const True)) (Decoders.maybeRow (Decoders.value Decoders.bool))
      session =
        do
          result1 <- Session.query [1, 2] query
          result2 <- Session.query [2, 3] query
          return (result1, result2)
      in do
        x <- Connection.with (Session.run session)
        assertEqual (show x) (Right (Right (True, False))) x
    ,
    testCase "Composite decoding" $
    let
      query =
        Query.statement sql encoder decoder True
        where
          sql =
            "select (1, true)"
          encoder = 
            Encoders.unit
          decoder =
            Decoders.singleRow (Decoders.value (Decoders.composite ((,) <$> Decoders.compositeValue Decoders.int8 <*> Decoders.compositeValue Decoders.bool)))
      session =
        Session.query () query
      in do
        x <- Connection.with (Session.run session)
        assertEqual (show x) (Right (Right (1, True))) x
    ,
    testCase "Complex composite decoding" $
    let
      query =
        Query.statement sql encoder decoder True
        where
          sql =
            "select (1, true) as entity1, ('hello', 3) as entity2"
          encoder =
            Encoders.unit
          entity1Decoder =
            (,) <$> Decoders.compositeValue Decoders.int8 <*> Decoders.compositeValue Decoders.bool
          entity2Decoder =
            (,) <$> Decoders.compositeValue Decoders.text <*> Decoders.compositeValue Decoders.int8
          decoder =
            Decoders.singleRow $ Decoders.value $ Decoders.composite
            ((,) <$> entity1Decoder <*> entity2Decoder)
      session =
        Session.query () query
      in do
        x <- Connection.with (Session.run session)
        assertEqual (show x) (Right (Right ((1, True), ("hello", 3)))) x
    ,
    testCase "Empty array" $
    let
      io =
        do
          x <- Connection.with (Session.run session)
          assertEqual (show x) (Right (Right [])) x
        where
          session =
            Session.query () query
            where
              query =
                Query.statement sql encoder decoder True
                where
                  sql =
                    "select array[]::int8[]"
                  encoder =
                    Encoders.unit
                  decoder =
                    Decoders.singleRow (Decoders.value (Decoders.array (Decoders.arrayDimension replicateM (Decoders.arrayValue Decoders.int8))))
      in io
    ,
    testCase "Failing prepared statements" $
    let
      io =
        Connection.with (Session.run session) >>=
        (assertBool <$> show <*> resultTest)
        where
          resultTest =
            \case
              Right (Left (Session.ResultError (Session.ServerError "26000" _ _ _))) -> False
              _ -> True
          session =
            catchError session (const (pure ())) *> session
            where
              session =
                Session.query () query
                where
                  query =
                    Query.statement sql encoder decoder True
                    where
                      sql =
                        "absurd"
                      encoder =
                        Encoders.unit
                      decoder =
                        Decoders.unit
      in io
    ,
    testCase "Prepared statements after error" $
    let
      io =
        Connection.with (Session.run session) >>=
        \x -> assertBool (show x) (either (const False) isRight x)
        where
          session =
            try *> fail *> try
            where
              try =
                Session.query 1 query
                where
                  query =
                    Query.statement sql encoder decoder True
                    where
                      sql =
                        "select $1 :: int8"
                      encoder =
                        Encoders.value Encoders.int8
                      decoder =
                        Decoders.singleRow $ Decoders.value Decoders.int8
              fail =
                catchError (Session.sql "absurd") (const (pure ()))
      in io
    ,
    testCase "\"in progress after error\" bugfix" $
    let
      sumQuery :: Query.Query (Int64, Int64) Int64
      sumQuery =
        Query.statement sql encoder decoder True
        where
          sql =
            "select ($1 + $2)"
          encoder =
            contramap fst (Encoders.value Encoders.int8) <>
            contramap snd (Encoders.value Encoders.int8)
          decoder =
            Decoders.singleRow (Decoders.value Decoders.int8)
      sumSession :: Session.Session Int64
      sumSession =
        Session.sql "begin" *> Session.query (1, 1) sumQuery <* Session.sql "end"
      errorSession :: Session.Session ()
      errorSession =
        Session.sql "asldfjsldk"
      io =
        Connection.with $ \c -> do
          Session.run errorSession c
          Session.run sumSession c
      in io >>= \x -> assertBool (show x) (either (const False) isRight x)
    ,
    testCase "\"another command is already in progress\" bugfix" $
    let
      sumQuery :: Query.Query (Int64, Int64) Int64
      sumQuery =
        Query.statement sql encoder decoder True
        where
          sql =
            "select ($1 + $2)"
          encoder =
            contramap fst (Encoders.value Encoders.int8) <>
            contramap snd (Encoders.value Encoders.int8)
          decoder =
            Decoders.singleRow (Decoders.value Decoders.int8)
      session :: Session.Session Int64
      session =
        do
          Session.sql "begin;"
          s <- Session.query (1,1) sumQuery
          Session.sql "end;"
          return s
      in DSL.session session >>= \x -> assertEqual (show x) (Right 2) x
    ,
    testCase "Executing the same query twice" $
    pure ()
    ,
    testCase "Interval Encoding" $
    let
      actualIO =
        DSL.session $ do
          let
            query =
              Query.statement sql encoder decoder True
              where
                sql =
                  "select $1 = interval '10 seconds'"
                decoder =
                  (Decoders.singleRow (Decoders.value (Decoders.bool)))
                encoder =
                  Encoders.value (Encoders.interval)
            in DSL.query (10 :: DiffTime) query
      in actualIO >>= \x -> assertEqual (show x) (Right True) x
    ,
    testCase "Interval Decoding" $
    let
      actualIO =
        DSL.session $ do
          let
            query =
              Query.statement sql encoder decoder True
              where
                sql =
                  "select interval '10 seconds'"
                decoder =
                  (Decoders.singleRow (Decoders.value (Decoders.interval)))
                encoder =
                  Encoders.unit
            in DSL.query () query
      in actualIO >>= \x -> assertEqual (show x) (Right (10 :: DiffTime)) x
    ,
    testCase "Interval Encoding/Decoding" $
    let
      actualIO =
        DSL.session $ do
          let
            query =
              Query.statement sql encoder decoder True
              where
                sql =
                  "select $1"
                decoder =
                  (Decoders.singleRow (Decoders.value (Decoders.interval)))
                encoder =
                  Encoders.value (Encoders.interval)
            in DSL.query (10 :: DiffTime) query
      in actualIO >>= \x -> assertEqual (show x) (Right (10 :: DiffTime)) x
    ,
    testCase "Unknown" $
    let
      actualIO =
        DSL.session $ do
          let
            query =
              Query.statement sql mempty Decoders.unit True
              where
                sql =
                  "drop type if exists mood"
            in DSL.query () query
          let
            query =
              Query.statement sql mempty Decoders.unit True
              where
                sql =
                  "create type mood as enum ('sad', 'ok', 'happy')"
            in DSL.query () query
          let
            query =
              Query.statement sql encoder decoder True
              where
                sql =
                  "select $1 = ('ok' :: mood)"
                decoder =
                  (Decoders.singleRow (Decoders.value (Decoders.bool)))
                encoder =
                  Encoders.value (Encoders.unknown)
            in DSL.query "ok" query
      in actualIO >>= assertEqual "" (Right True)
    ,
    testCase "Enum" $
    let
      actualIO =
        DSL.session $ do
          let
            query =
              Query.statement sql mempty Decoders.unit True
              where
                sql =
                  "drop type if exists mood"
            in DSL.query () query
          let
            query =
              Query.statement sql mempty Decoders.unit True
              where
                sql =
                  "create type mood as enum ('sad', 'ok', 'happy')"
            in DSL.query () query
          let
            query =
              Query.statement sql encoder decoder True
              where
                sql =
                  "select ($1 :: mood)"
                decoder =
                  (Decoders.singleRow (Decoders.value (Decoders.enum (Just . id))))
                encoder =
                  Encoders.value (Encoders.enum id)
            in DSL.query "ok" query
      in actualIO >>= assertEqual "" (Right "ok")
    ,
    testCase "The same prepared statement used on different types" $ 
    let
      actualIO =
        DSL.session $ do
          let
            effect1 =
              DSL.query "ok" query
              where
                query =
                  Query.statement sql encoder decoder True
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
                  Query.statement sql encoder decoder True
                  where
                    sql =
                      "select $1"
                    encoder =
                      Encoders.value Encoders.int8
                    decoder =
                      (Decoders.singleRow (Decoders.value Decoders.int8))
            in (,) <$> effect1 <*> effect2
      in actualIO >>= assertEqual "" (Right ("ok", 1))
    ,
    testCase "Affected rows counting" $
    replicateM_ 13 $
    let
      actualIO =
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
            DSL.query () $ Query.statement sql def decoder False
            where
              sql =
                "delete from a"
              decoder =
                Decoders.rowsAffected
      in actualIO >>= assertEqual "" (Right 100)
    ,
    testCase "Result of an auto-incremented column" $
    let
      actualIO =
        DSL.session $ do
          DSL.query () $ Queries.plain $ "drop table if exists a"
          DSL.query () $ Queries.plain $ "create table a (id serial not null, v char not null, primary key (id))"
          id1 <- DSL.query () $ Query.statement "insert into a (v) values ('a') returning id" def (Decoders.singleRow (Decoders.value Decoders.int4)) False
          id2 <- DSL.query () $ Query.statement "insert into a (v) values ('b') returning id" def (Decoders.singleRow (Decoders.value Decoders.int4)) False
          DSL.query () $ Queries.plain $ "drop table if exists a"
          pure (id1, id2)
      in assertEqual "" (Right (1, 2)) =<< actualIO
    ,
    testCase "List decoding" $
    let
      actualIO =
        DSL.session $ DSL.query () $ Queries.selectList
      in assertEqual "" (Right [(1, 2), (3, 4), (5, 6)]) =<< actualIO 
  ]

