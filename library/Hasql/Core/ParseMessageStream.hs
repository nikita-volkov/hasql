module Hasql.Core.ParseMessageStream where

import Hasql.Prelude hiding (error)
import Hasql.Core.Model
import qualified Hasql.Core.ParseMessage as A
import qualified Hasql.Core.ParseDataRow as F
import qualified Hasql.Core.MessageTypePredicates as G
import qualified Hasql.Protocol.Decoding as E
import qualified Hasql.Protocol.Model as C
import qualified BinaryParser as D


{-|
A specification of how to parse a stream of messages.
-}
newtype ParseMessageStream result =
  ParseMessageStream (A.ParseMessage (Either result (ParseMessageStream result)))

instance Functor ParseMessageStream where
  {-# INLINE fmap #-}
  fmap mapping (ParseMessageStream parseMessage) =
    ParseMessageStream (fmap (either (Left . mapping) (Right . fmap mapping)) parseMessage)

instance Applicative ParseMessageStream where
  {-# INLINE pure #-}
  pure x =
    ParseMessageStream (pure (Left x))
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Alternative ParseMessageStream where
  {-# INLINE empty #-}
  empty =
    ParseMessageStream empty
  {-# INLINE (<|>) #-}
  (<|>) (ParseMessageStream left) (ParseMessageStream right) =
    ParseMessageStream (left' <|> right')
    where
      left' =
        fmap (fmap (<|> ParseMessageStream right)) left
      right' =
        fmap (fmap (ParseMessageStream left <|>)) right

instance Monad ParseMessageStream where
  {-# INLINE return #-}
  return = pure
  {-# INLINE (>>=) #-}
  (>>=) (ParseMessageStream left) rightK =
    ParseMessageStream (fmap mapping left)
    where
      mapping =
        \case
          Left leftOutput -> Right (rightK leftOutput)
          Right (ParseMessageStream nextLeft) -> Right (ParseMessageStream (fmap mapping nextLeft))

instance MonadPlus ParseMessageStream where
  mzero = empty
  mplus = (<|>)


parseMessage :: A.ParseMessage result -> ParseMessageStream result
parseMessage parseMessage =
  ParseMessageStream (fmap Left parseMessage)

error :: ParseMessageStream (Either Text ErrorMessage)
error =
  parseMessage A.error

commandComplete :: ParseMessageStream (Either Text Int)
commandComplete =
  parseMessage A.commandComplete

rows :: F.ParseDataRow row -> Fold row result -> ParseMessageStream (Either Text result)
rows parseDataRow (Fold foldStep foldStart foldEnd) =
  fold foldStart
  where
    fold state =
      ParseMessageStream (step <|> end)
      where
        step =
          fmap fromParsingResult (A.dataRow (E.parseDataRow parseDataRow))
          where
            fromParsingResult =
              \case
                Left error -> Left (Left error)
                Right row -> Right (fold (foldStep state row))
        end =
          (A.commandCompleteWithoutAmount <|> A.emptyQuery) $>
          Left (Right (foldEnd state))

rowsAffected :: ParseMessageStream (Either Text Int)
rowsAffected =
  ParseMessageStream (commandComplete <|> dataRow <|> emptyQuery)
  where
    commandComplete =
      Left <$> A.commandComplete
    dataRow =
      Right rowsAffected <$ A.dataRowWithoutData
    emptyQuery =
      Left (Right 0) <$ A.emptyQuery

parseComplete :: ParseMessageStream ()
parseComplete =
  parseMessage A.parseComplete

bindComplete :: ParseMessageStream ()
bindComplete =
  parseMessage A.bindComplete

readyForQuery :: ParseMessageStream ()
readyForQuery =
  parseMessage A.readyForQuery

authentication :: ParseMessageStream (Either Text (Either ErrorMessage AuthenticationResult))
authentication =
  iterate (Left "Missing the \"integer_datetimes\" setting")
  where
    iterate state =
      ParseMessageStream (param <|> authentication <|> error)
      where
        param =
          fromParsingResult <$> A.parameterStatus
          where
            fromParsingResult = 
              \case
                Left error -> Left (Left error)
                Right (name, value) -> case name of
                  "integer_datetimes" -> case value of
                    "on" -> Right (iterate (Right True))
                    "off" -> Right (iterate (Right False))
                    _ -> Right (iterate (Left ("Unexpected value of the \"integer_datetimes\" setting: " <> (fromString . show) value)))
                  _ -> Right (iterate state)
        authentication =
          fromParsingResult <$> A.authentication
          where
            fromParsingResult =
              \case
                Left error -> Left (Left error)
                Right authentication -> case authentication of
                  C.OkAuthenticationMessage -> Right (ParseMessageStream readyForQuery)
                  C.ClearTextPasswordAuthenticationMessage -> Left (Right (Right NeedClearTextPasswordAuthenticationResult))
                  C.MD5PasswordAuthenticationMessage salt -> Left (Right (Right (NeedMD5PasswordAuthenticationResult salt)))
        error =
          fmap (Left . either Left (Right . Left)) A.error
        readyForQuery =
          Left (Right . OkAuthenticationResult <$> state) <$ A.readyForQuery
