module Main where

import Main.Prelude hiding (assert)
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck as QuickCheck
import qualified Main.Statements as Statements
import qualified Main.DSL as DSL
import qualified Main.Connection as Connection
import qualified Hasql.Statement as Statement
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Session as Session

main =
  defaultMain tree

tree =
  localOption (NumThreads 1) $
  testGroup "All tests"
  [
    testCase "Failed query" $
    let
      statement =
        Statement.Statement "select true where 1 = any ($1) and $2" encoder decoder True
        where
          encoder =
            contrazip2
              (Encoders.param (Encoders.array (Encoders.dimension foldl' (Encoders.element Encoders.int8))))
              (Encoders.param Encoders.text)
          decoder =
            fmap (maybe False (const True)) (Decoders.rowMaybe (Decoders.column Decoders.bool))
      session =
        Session.statement ([3, 7], "a") statement
      in do
        x <- Connection.with (Session.run session)
        assertBool (show x) $ case x of
          Right (Left (Session.QueryError "select true where 1 = any ($1) and $2" ["[3, 7]", "\"a\""] _)) -> True
          _ -> False
    ,
    testCase "IN simulation" $
    let
      statement =
        Statement.Statement "select true where 1 = any ($1)" encoder decoder True
        where
          encoder =
            Encoders.param (Encoders.array (Encoders.dimension foldl' (Encoders.element Encoders.int8)))
          decoder =
            fmap (maybe False (const True)) (Decoders.rowMaybe (Decoders.column Decoders.bool))
      session =
        do
          result1 <- Session.statement [1, 2] statement
          result2 <- Session.statement [2, 3] statement
          return (result1, result2)
      in do
        x <- Connection.with (Session.run session)
        assertEqual (show x) (Right (Right (True, False))) x
    ,
    testCase "NOT IN simulation" $
    let
      statement =
        Statement.Statement "select true where 3 <> all ($1)" encoder decoder True
        where
          encoder =
            Encoders.param (Encoders.array (Encoders.dimension foldl' (Encoders.element Encoders.int8)))
          decoder =
            fmap (maybe False (const True)) (Decoders.rowMaybe (Decoders.column Decoders.bool))
      session =
        do
          result1 <- Session.statement [1, 2] statement
          result2 <- Session.statement [2, 3] statement
          return (result1, result2)
      in do
        x <- Connection.with (Session.run session)
        assertEqual (show x) (Right (Right (True, False))) x
    ,
    testCase "Composite decoding" $
    let
      statement =
        Statement.Statement sql encoder decoder True
        where
          sql =
            "select (1, true)"
          encoder = 
            Encoders.unit
          decoder =
            Decoders.singleRow (Decoders.column (Decoders.composite ((,) <$> Decoders.field Decoders.int8 <*> Decoders.field Decoders.bool)))
      session =
        Session.statement () statement
      in do
        x <- Connection.with (Session.run session)
        assertEqual (show x) (Right (Right (1, True))) x
    ,
    testCase "Complex composite decoding" $
    let
      statement =
        Statement.Statement sql encoder decoder True
        where
          sql =
            "select (1, true) as entity1, ('hello', 3) as entity2"
          encoder =
            Encoders.unit
          decoder =
            Decoders.singleRow $
            (,) <$> Decoders.column entity1 <*> Decoders.column entity2
            where
              entity1 =
                Decoders.composite $
                (,) <$> Decoders.field Decoders.int8 <*> Decoders.field Decoders.bool
              entity2 =
                Decoders.composite $
                (,) <$> Decoders.field Decoders.text <*> Decoders.field Decoders.int8
      session =
        Session.statement () statement
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
            Session.statement () statement
            where
              statement =
                Statement.Statement sql encoder decoder True
                where
                  sql =
                    "select array[]::int8[]"
                  encoder =
                    Encoders.unit
                  decoder =
                    Decoders.singleRow (Decoders.column (Decoders.array (Decoders.dimension replicateM (Decoders.element Decoders.int8))))
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
              Right (Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError "26000" _ _ _)))) -> False
              _ -> True
          session =
            catchError session (const (pure ())) *> session
            where
              session =
                Session.statement () statement
                where
                  statement =
                    Statement.Statement sql encoder decoder True
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
                Session.statement 1 statement
                where
                  statement =
                    Statement.Statement sql encoder decoder True
                    where
                      sql =
                        "select $1 :: int8"
                      encoder =
                        Encoders.param Encoders.int8
                      decoder =
                        Decoders.singleRow $ Decoders.column Decoders.int8
              fail =
                catchError (Session.sql "absurd") (const (pure ()))
      in io
    ,
    testCase "\"in progress after error\" bugfix" $
    let
      sumStatement :: Statement.Statement (Int64, Int64) Int64
      sumStatement =
        Statement.Statement sql encoder decoder True
        where
          sql =
            "select ($1 + $2)"
          encoder =
            contramap fst (Encoders.param Encoders.int8) <>
            contramap snd (Encoders.param Encoders.int8)
          decoder =
            Decoders.singleRow (Decoders.column Decoders.int8)
      sumSession :: Session.Session Int64
      sumSession =
        Session.sql "begin" *> Session.statement (1, 1) sumStatement <* Session.sql "end"
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
      sumStatement :: Statement.Statement (Int64, Int64) Int64
      sumStatement =
        Statement.Statement sql encoder decoder True
        where
          sql =
            "select ($1 + $2)"
          encoder =
            contramap fst (Encoders.param Encoders.int8) <>
            contramap snd (Encoders.param Encoders.int8)
          decoder =
            Decoders.singleRow (Decoders.column Decoders.int8)
      session :: Session.Session Int64
      session =
        do
          Session.sql "begin;"
          s <- Session.statement (1,1) sumStatement
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
            statement =
              Statement.Statement sql encoder decoder True
              where
                sql =
                  "select $1 = interval '10 seconds'"
                decoder =
                  (Decoders.singleRow (Decoders.column (Decoders.bool)))
                encoder =
                  Encoders.param (Encoders.interval)
            in DSL.statement (10 :: DiffTime) statement
      in actualIO >>= \x -> assertEqual (show x) (Right True) x
    ,
    testCase "Interval Decoding" $
    let
      actualIO =
        DSL.session $ do
          let
            statement =
              Statement.Statement sql encoder decoder True
              where
                sql =
                  "select interval '10 seconds'"
                decoder =
                  (Decoders.singleRow (Decoders.column (Decoders.interval)))
                encoder =
                  Encoders.unit
            in DSL.statement () statement
      in actualIO >>= \x -> assertEqual (show x) (Right (10 :: DiffTime)) x
    ,
    testCase "Interval Encoding/Decoding" $
    let
      actualIO =
        DSL.session $ do
          let
            statement =
              Statement.Statement sql encoder decoder True
              where
                sql =
                  "select $1"
                decoder =
                  (Decoders.singleRow (Decoders.column (Decoders.interval)))
                encoder =
                  Encoders.param (Encoders.interval)
            in DSL.statement (10 :: DiffTime) statement
      in actualIO >>= \x -> assertEqual (show x) (Right (10 :: DiffTime)) x
    ,
    testCase "Unknown" $
    let
      actualIO =
        DSL.session $ do
          let
            statement =
              Statement.Statement sql mempty Decoders.unit True
              where
                sql =
                  "drop type if exists mood"
            in DSL.statement () statement
          let
            statement =
              Statement.Statement sql mempty Decoders.unit True
              where
                sql =
                  "create type mood as enum ('sad', 'ok', 'happy')"
            in DSL.statement () statement
          let
            statement =
              Statement.Statement sql encoder decoder True
              where
                sql =
                  "select $1 = ('ok' :: mood)"
                decoder =
                  (Decoders.singleRow (Decoders.column (Decoders.bool)))
                encoder =
                  Encoders.param (Encoders.unknown)
            in DSL.statement "ok" statement
      in actualIO >>= assertEqual "" (Right True)
    ,
    testCase "Textual Unknown" $
    let
      actualIO =
        DSL.session $ do
          let
            statement =
              Statement.Statement sql mempty Decoders.unit True
              where
                sql =
                  "create or replace function overloaded(a int, b int) returns int as $$ select a + b $$ language sql;"
            in DSL.statement () statement
          let
            statement =
              Statement.Statement sql mempty Decoders.unit True
              where
                sql =
                  "create or replace function overloaded(a text, b text, c text) returns text as $$ select a || b || c $$ language sql;"
            in DSL.statement () statement
          let
            statement =
              Statement.Statement sql encoder decoder True
              where
                sql =
                  "select overloaded($1, $2) || overloaded($3, $4, $5)"
                decoder =
                  (Decoders.singleRow (Decoders.column (Decoders.text)))
                encoder =
                  contramany (Encoders.param Encoders.unknown)
            in DSL.statement ["1", "2", "4", "5", "6"] statement
      in actualIO >>= assertEqual "" (Right "3456")
    ,
    testCase "Enum" $
    let
      actualIO =
        DSL.session $ do
          let
            statement =
              Statement.Statement sql mempty Decoders.unit True
              where
                sql =
                  "drop type if exists mood"
            in DSL.statement () statement
          let
            statement =
              Statement.Statement sql mempty Decoders.unit True
              where
                sql =
                  "create type mood as enum ('sad', 'ok', 'happy')"
            in DSL.statement () statement
          let
            statement =
              Statement.Statement sql encoder decoder True
              where
                sql =
                  "select ($1 :: mood)"
                decoder =
                  (Decoders.singleRow (Decoders.column (Decoders.enum (Just . id))))
                encoder =
                  Encoders.param (Encoders.enum id)
            in DSL.statement "ok" statement
      in actualIO >>= assertEqual "" (Right "ok")
    ,
    testCase "The same prepared statement used on different types" $ 
    let
      actualIO =
        DSL.session $ do
          let
            effect1 =
              DSL.statement "ok" statement
              where
                statement =
                  Statement.Statement sql encoder decoder True
                  where
                    sql =
                      "select $1"
                    encoder =
                      Encoders.param Encoders.text
                    decoder =
                      (Decoders.singleRow (Decoders.column (Decoders.text)))
            effect2 =
              DSL.statement 1 statement
              where
                statement =
                  Statement.Statement sql encoder decoder True
                  where
                    sql =
                      "select $1"
                    encoder =
                      Encoders.param Encoders.int8
                    decoder =
                      (Decoders.singleRow (Decoders.column Decoders.int8))
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
            DSL.statement () $ Statements.plain $ 
            "drop table if exists a"
          createTable =
            DSL.statement () $ Statements.plain $
            "create table a (id bigserial not null, name varchar not null, primary key (id))"
          insertRow =
            DSL.statement () $ Statements.plain $
            "insert into a (name) values ('a')"  
          deleteRows =
            DSL.statement () $ Statement.Statement sql def decoder False
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
          DSL.statement () $ Statements.plain $ "drop table if exists a"
          DSL.statement () $ Statements.plain $ "create table a (id serial not null, v char not null, primary key (id))"
          id1 <- DSL.statement () $ Statement.Statement "insert into a (v) values ('a') returning id" def (Decoders.singleRow (Decoders.column Decoders.int4)) False
          id2 <- DSL.statement () $ Statement.Statement "insert into a (v) values ('b') returning id" def (Decoders.singleRow (Decoders.column Decoders.int4)) False
          DSL.statement () $ Statements.plain $ "drop table if exists a"
          pure (id1, id2)
      in assertEqual "" (Right (1, 2)) =<< actualIO
    ,
    testCase "List decoding" $
    let
      actualIO =
        DSL.session $ DSL.statement () $ Statements.selectList
      in assertEqual "" (Right [(1, 2), (3, 4), (5, 6)]) =<< actualIO
    ,
    testCase "Multi param statement query" $
    let
      filters = [TextFilter "some text", IntegerFilter 42]
      filterEnc = [contramap (\(TextFilter t) -> t) (Encoders.param Encoders.text), contramap (\(IntegerFilter i) -> fromIntegral i) (Encoders.param Encoders.int4)]
      actualIO =
        DSL.session $ do
          DSL.statement () $ Statements.plain $ "drop table if exists s_s"
          DSL.statement () $ Statements.plain $ "drop table if exists s_p"
          DSL.statement () $ Statements.plain $ "drop table if exists s_i"
          DSL.statement () $ Statements.plain $ "drop table if exists s_t"
          DSL.statement () $ Statements.plain $ "create table s_s (id int4 not null, s text null)"
          DSL.statement () $ Statements.plain $ "create table s_t (id int4 not null, t text null)"
          DSL.statement () $ Statements.plain $ "create table s_i (id int4 not null, i integer null)"
          DSL.statement () $ Statements.plain $ "create table s_p (sid int4 not null, pid int4 not null)"
          DSL.statement () $ Statements.plain $ "insert into s_s values (1, 'subject') returning id"
          DSL.statement () $ Statements.plain $ "insert into s_t values (2, 'some text') returning id"
          DSL.statement () $ Statements.plain $ "insert into s_i values (3, 42) returning id"
          DSL.statement () $ Statements.plain $ "insert into s_p(sid, pid) values(1,2)"
          DSL.statement () $ Statements.plain $ "insert into s_p(sid, pid) values(1,3)" 
          txt <- Session.multiParamStatement filters $ Statement.MultiParamStatement "with o1 as (select id from s_t where t like $1), o2 as (select id from s_i where i = $2), s1 as (select sid from s_p join o1 on s_p.pid = o1.id), s2 as (select sid from s_p join o2 on s_p.pid = o2.id) select s from s_s, s1, s2 where s_s.id =s1.sid and s_s.id = s2.sid" 
                                                  filterEnc (Decoders.singleRow (Decoders.column Decoders.text)) False
          DSL.statement () $ Statements.plain $ "drop table if exists s_s"
          DSL.statement () $ Statements.plain $ "drop table if exists s_p"
          DSL.statement () $ Statements.plain $ "drop table if exists s_i"
          DSL.statement () $ Statements.plain $ "drop table if exists s_t"
          pure txt
      in assertEqual "" (Right ("subject")) =<< actualIO
  ]

data Fitler = TextFilter Text | IntegerFilter Integer