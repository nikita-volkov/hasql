module Main where

import Main.Prelude
import Criterion.Main
import qualified Hasql as H
import qualified Hasql.Encoding as HE
import qualified Hasql.Decoding as HD
import qualified Main.Queries as Q


main =
  H.connect settings >>= either (fail . show) use
  where
    settings =
      H.ParametricSettings host port user password database
      where
        host = "localhost"
        port = 5432
        user = "postgres"
        password = ""
        database = "postgres"
    use connection =
      defaultMain
      [
        bgroup "decoding"
        [
          bgroup "1 column"
          [
            bench "1 row" $ nfIO $! query () $! Q.select1 1
            ,
            bench "100 rows" $ nfIO $! query () $! Q.select1 100
            ,
            bench "10000 rows" $ nfIO $! query () $! Q.select1 10000
          ]
          ,
          bgroup "4 columns"
          [
            bench "1 row" $ nfIO $! query () $! Q.select4 1
            ,
            bench "100 rows" $ nfIO $! query () $! Q.select4 100
            ,
            bench "10000 rows" $ nfIO $! query () $! Q.select4 10000
          ]
        ]
      ]
      where
        query :: a -> H.Query a b -> IO b
        query params query =
          {-# SCC "query" #-} 
          H.query connection query params >>= either (fail . show) pure
