module HighSQL.QQ where

import HighSQL.Prelude
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified HighSQL.QQ.Parser as Parser
import qualified HighSQL.API as API
import qualified HighSQL.Backend as Backend
import qualified HighSQL.Conversion as Conversion


-- |
-- Produces a lambda-expression, 
-- which takes as many parameters as there are placeholders in the statement
-- and, depending on the statement kind, results in one of the following types:
-- 
-- [@SELECT@]
-- 
-- A stream of results, with 'API.T' as the inner monad:
-- 
-- @'API.ResultsStream' s ('API.T' l s) r@
-- 
-- [@INSERT@]
-- 
-- A transaction,
-- which returns the possibly auto-incremented value
-- (of the @id@ column typically):
-- 
-- @'API.T' l s (Maybe Integer)@
-- 
-- [@UPDATE, DELETE@]
-- 
-- A transaction, 
-- which returns the amount of affected rows:
-- 
-- @'API.T' l s Integer@
-- 
-- [@CREATE, ALTER, DROP, TRUNCATE@]
-- 
-- A unit transaction:
-- 
-- @'API.T' l s ()@
-- 
-- Example:
-- 
-- >write session $ do
-- >  artistIDMaybe <- ListT.head $ [q| SELECT id FROM artists WHERE name = ? |] "Metallica" 
-- >  userIDMaybe   <- ListT.head $ [q| SELECT id FROM users   WHERE name = ? |] "Nikita Volkov"
-- >  forM_ ((,) <$> artistIDMaybe <*> userIDMaybe) $ \(artistID, userID) ->
-- >    [q| INSERT INTO artists_fans (artist_id, user_id) VALUES (?, ?) |] artistID userID
-- 
-- Of course, the same thing can be implemented a bit smarter:
-- 
-- >write session $ sequence_ $ ListT.head $ do
-- >  artistID <- [q| SELECT id FROM artists WHERE name = ? |] "Metallica" 
-- >  userID   <- [q| SELECT id FROM users   WHERE name = ? |] "Nikita Volkov"
-- >  return $ [q| INSERT INTO artists_fans (artist_id, user_id) VALUES (?, ?) |] artistID userID
-- 
-- In both examples above we execute a 'write' transaction,
-- in which we query two tables and insert a row into a third one.
q :: QuasiQuoter
q = 
  QuasiQuoter
    parseExp
    (const $ fail "Pattern context is not supported")
    (const $ fail "Type context is not supported")
    (const $ fail "Declaration context is not supported")


parseExp :: String -> Q Exp
parseExp s =
  do
    (k, n) <- 
      either (fail . (showString "Parsing failure: ")) return (Parser.parse (fromString s))
    return $ fmapLamE (AppE (VarE (conName k))) (statementF s n)
  where
    conName =
      \case
        Parser.Select   -> 'API.select
        Parser.Update   -> 'API.update
        Parser.Insert   -> 'API.insert
        Parser.Delete   -> 'API.update
        Parser.Create   -> 'API.create
        Parser.Alter    -> 'API.create
        Parser.Drop     -> 'API.create
        Parser.Truncate -> 'API.create

-- |
-- An expression of an arbitrary arg-length function, 
-- which produces a "API.Statement".
statementF :: String -> Int -> Exp
statementF s n =
  LamE pats exp
  where
    vars = map (mkName . ('_' :) . show) [1 .. n]
    pats = map VarP vars
    exp  = AppE (AppE (ConE 'API.Statement) (LitE (StringL s))) (ListE exps)
      where
        exps = map (AppE (VarE 'Conversion.toValue) . VarE) vars

fmapLamE :: (Exp -> Exp) -> Exp -> Exp
fmapLamE f =
  \case
    LamE pats exp -> LamE pats (f exp)
