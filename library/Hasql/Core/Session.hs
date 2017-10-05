module Hasql.Core.Session where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Core.Request as A


newtype Session result =
  Session (Free A.Request result)
  deriving (Functor, Applicative, Monad)

{-# INLINE startUp #-}
startUp :: ByteString -> Maybe ByteString -> [(ByteString, ByteString)] -> Session (Either ErrorMessage AuthenticationResult)
startUp username databaseMaybe runtimeParameters =
  Session (liftF (A.startUp username databaseMaybe runtimeParameters))

{-# INLINE clearTextPassword #-}
clearTextPassword :: ByteString -> Session (Either ErrorMessage AuthenticationResult)
clearTextPassword password =
  Session (liftF (A.clearTextPassword password))

{-# INLINE md5Password #-}
md5Password :: ByteString -> ByteString -> ByteString -> Session (Either ErrorMessage AuthenticationResult)
md5Password username password salt =
  Session (liftF (A.md5Password username password salt))

{-# INLINE handshake #-}
handshake :: ByteString -> ByteString -> Maybe ByteString -> [(ByteString, ByteString)] -> Session (Either Text (Either ErrorMessage Bool))
handshake username password databaseMaybe runtimeParameters =
  startUp username databaseMaybe runtimeParameters >>= handleFirstErrorOrAuthenticationResult
  where
    handleFirstErrorOrAuthenticationResult =
      \case
        Left error -> return (Right (Left error))
        Right authenticationResult -> case authenticationResult of
          OkAuthenticationResult idt -> return (Right (Right idt))
          NeedClearTextPasswordAuthenticationResult -> clearTextPassword password >>= handleSecondErrorOrAuthenticationResult
          NeedMD5PasswordAuthenticationResult salt -> md5Password username password salt >>= handleSecondErrorOrAuthenticationResult
    handleSecondErrorOrAuthenticationResult =
      \case
        Left error -> return (Right (Left error))
        Right authenticationResult -> case authenticationResult of
          OkAuthenticationResult idt -> return (Right (Right idt))
          _ -> return (Left "Can't authenticate")

{-# INLINE request #-}
request :: A.Request result -> Session result
request = Session . liftF
