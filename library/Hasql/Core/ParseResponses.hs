module Hasql.Core.ParseResponses where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Core.MessageTypePredicates as G
import qualified Hasql.Core.MessageTypeNames as H
import qualified Hasql.Core.ParseDataRow as A
import qualified Hasql.Core.ParseResponse as F
import qualified Data.Vector as B
import qualified Hasql.Core.Protocol.Parse.Responses as E
import qualified Ptr.Parse as C
import qualified Ptr.ByteString as D


newtype ParseResponses output =
  ParseResponses (ExceptT Text (F F.ParseResponse) output)
  deriving (Applicative, Monad, MonadError Text)

instance Functor ParseResponses where
  {-# INLINE fmap #-}
  fmap fn (ParseResponses (ExceptT free)) =
    {-# SCC "fmap" #-} 
    ParseResponses (ExceptT (fmap (fmap fn) free))

{-# INLINE parseResponse #-}
parseResponse :: F.ParseResponse output -> ParseResponses output
parseResponse pr =
  ParseResponses (lift (liftF pr))

{-# INLINE foldRows #-}
foldRows :: forall row output. Fold row output -> A.ParseDataRow row -> ParseResponses (output, Int)
foldRows (Fold foldStep foldStart foldEnd) pdr =
  ParseResponses $ ExceptT $ F $ \ pure lift ->
  let
    iterate !state =
      lift $
      F.rowOrEnd
        (fmap (\ !row -> iterate (foldStep state row)) pdr)
        (\ !amount -> pure (Right (foldEnd state, amount)))
    in 
      iterate foldStart

{-# INLINE singleRow #-}
singleRow :: A.ParseDataRow row -> ParseResponses row
singleRow pdr =
  ParseResponses $ ExceptT $ F $ \ pure lift ->
  let
    iterate !state =
      lift $
      F.rowOrEnd
        (fmap (\ !row -> iterate (Right row)) pdr)
        (\ _ -> pure state)
    in iterate (Left "Not a single row")

{-# INLINE rowsAffected #-}
rowsAffected :: ParseResponses Int
rowsAffected =
  parseResponse F.rowsAffected

{-# INLINE authenticationStatus #-}
authenticationStatus :: ParseResponses AuthenticationStatus
authenticationStatus =
  parseResponse F.authenticationStatus

{-# INLINE parameters #-}
parameters :: ParseResponses Bool
parameters =
  ParseResponses $ ExceptT $ F $ \ pure lift ->
  let
    iterate !state =
      lift $ parameterStatus <|> readyForQuery
      where
        parameterStatus =
          F.parameterStatus $ \ name value ->
          if name == "integer_datetimes"
            then case value of
              "on" -> iterate (Right True)
              "off" -> iterate (Right False)
              _ -> iterate (Left ("Unexpected value of the \"integer_datetimes\" setting: " <> (fromString . show) value))
            else iterate state
        readyForQuery =
          F.readyForQuery $> pure state
    in iterate (Left "Missing the \"integer_datetimes\" setting")

{-# INLINE authenticationResult #-}
authenticationResult :: ParseResponses AuthenticationResult
authenticationResult =
  do
    authenticationStatusResult <- authenticationStatus
    case authenticationStatusResult of
      NeedClearTextPasswordAuthenticationStatus -> return (NeedClearTextPasswordAuthenticationResult)
      NeedMD5PasswordAuthenticationStatus salt -> return (NeedMD5PasswordAuthenticationResult salt)
      OkAuthenticationStatus -> OkAuthenticationResult <$> parameters

{-# INLINE parseComplete #-}
parseComplete :: ParseResponses ()
parseComplete =
  parseResponse F.parseComplete

{-# INLINE bindComplete #-}
bindComplete :: ParseResponses ()
bindComplete =
  parseResponse F.bindComplete

{-# INLINE readyForQuery #-}
readyForQuery :: ParseResponses ()
readyForQuery =
  parseResponse F.readyForQuery
