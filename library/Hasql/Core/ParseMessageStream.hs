module Hasql.Core.ParseMessageStream where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Core.ParseMessage as A
import qualified Hasql.Core.MessageTypePredicates as G
import qualified Hasql.Protocol.Decoding as E
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
  pure x =
    ParseMessageStream (pure (Left x))
  (<*>) (ParseMessageStream left) (ParseMessageStream right) =
    ParseMessageStream merged
    where
      merged =
        $(todo "<*>")

instance Alternative ParseMessageStream where
  empty =
    ParseMessageStream empty
  (<|>) (ParseMessageStream left) (ParseMessageStream right) =
    ParseMessageStream (left' <|> right')
    where
      left' =
        fmap (fmap (<|> ParseMessageStream right)) left
      right' =
        fmap (fmap (ParseMessageStream left <|>)) right

parseMessage :: A.ParseMessage result -> ParseMessageStream result
parseMessage parseMessage =
  ParseMessageStream (fmap Left parseMessage)

error :: ParseMessageStream (Either Text ErrorMessage)
error =
  parseMessage A.error

commandComplete :: ParseMessageStream (Either Text Int)
commandComplete =
  parseMessage A.commandComplete

rows :: D.BinaryParser row -> Fold row result -> ParseMessageStream (Either Text result)
rows rowParser (Fold foldStep foldStart foldEnd) =
  fold foldStart
  where
    fold state =
      ParseMessageStream (step <|> end)
      where
        step =
          fmap fromParsingResult (A.dataRow rowParser)
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
