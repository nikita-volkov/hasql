module Hasql.Protocol.Model where

import Hasql.Prelude


newtype DataFormat =
  DataFormat Word16

pattern TextDataFormat =
  DataFormat 0

pattern BinaryDataFormat =
  DataFormat 1


{-|
An encoded representation of the backend message type
-}
newtype MessageType =
  MessageType Word8
  deriving (Eq, Ord)

instance Show MessageType where
  show =
    \case
      AuthenticationMessageType -> "AuthenticationMessageType"
      KeyDataMessageType -> "KeyDataMessageType"
      BindCompleteMessageType -> "BindCompleteMessageType"
      CloseCompleteMessageType -> "CloseCompleteMessageType"
      CommandCompleteMessageType -> "CommandCompleteMessageType"
      CopyOutDataMessageType -> "CopyOutDataMessageType"
      CopyOutDoneMessageType -> "CopyOutDoneMessageType"
      CopyInResponseMessageType -> "CopyInResponseMessageType"
      CopyOutMessageType -> "CopyOutMessageType"
      CopyBothMessageType -> "CopyBothMessageType"
      DataRowMessageType -> "DataRowMessageType"
      EmptyQueryMessageType -> "EmptyQueryMessageType"
      ErrorMessageType -> "ErrorMessageType"
      FunctionCallMessageType -> "FunctionCallMessageType"
      NoDataMessageType -> "NoDataMessageType"
      NoticeMessageType -> "NoticeMessageType"
      NotificationMessageType -> "NotificationMessageType"
      ParameterDescriptionMessageType -> "ParameterDescriptionMessageType"
      ParameterStatusMessageType -> "ParameterStatusMessageType"
      ParseCompleteMessageType -> "ParseCompleteMessageType"
      PortalSuspendedMessageType -> "PortalSuspendedMessageType"
      ReadyForQueryMessageType -> "ReadyForQueryMessageType"
      RowDescriptionMessageType -> "RowDescriptionMessageType"
      MessageType x -> "MessageType " <> show x

pattern AuthenticationMessageType =
  MessageType 82

pattern KeyDataMessageType =
  MessageType 75

pattern BindCompleteMessageType =
  MessageType 50

pattern CloseCompleteMessageType =
  MessageType 51

pattern CommandCompleteMessageType =
  MessageType 67

pattern CopyOutDataMessageType =
  MessageType 100

pattern CopyOutDoneMessageType =
  MessageType 99

pattern CopyInResponseMessageType =
  MessageType 71

pattern CopyOutMessageType =
  MessageType 72

pattern CopyBothMessageType =
  MessageType 87

pattern DataRowMessageType =
  MessageType 68

pattern EmptyQueryMessageType =
  MessageType 73

pattern ErrorMessageType =
  MessageType 69

pattern FunctionCallMessageType =
  MessageType 86

pattern NoDataMessageType =
  MessageType 110

pattern NoticeMessageType =
  MessageType 78

pattern NotificationMessageType =
  MessageType 65

pattern ParameterDescriptionMessageType =
  MessageType 116

pattern ParameterStatusMessageType =
  MessageType 83

pattern ParseCompleteMessageType =
  MessageType 49

pattern PortalSuspendedMessageType =
  MessageType 115

pattern ReadyForQueryMessageType =
  MessageType 90

pattern RowDescriptionMessageType =
  MessageType 84


newtype PayloadLength =
  PayloadLength Int32

pattern NullPayloadLength =
  PayloadLength (-1)


data Payload =
  NullPayload |
  BytesPayload !ByteString


newtype NoticeFieldType =
  NoticeFieldType Word8

pattern CodeNoticeFieldType =
  NoticeFieldType 0x43

pattern ColumnNoticeFieldType =
  NoticeFieldType 0x63

pattern ConstraintNoticeFieldType =
  NoticeFieldType 0x6E

pattern ContextNoticeFieldType =
  NoticeFieldType 0x57

pattern DataTypeNoticeFieldType =
  NoticeFieldType 0x64

pattern DetailNoticeFieldType =
  NoticeFieldType 0x44

pattern FileNoticeFieldType =
  NoticeFieldType 0x46

pattern HintNoticeFieldType =
  NoticeFieldType 0x48

pattern InternalPositionNoticeFieldType =
  NoticeFieldType 0x70

pattern InternalQueryNoticeFieldType =
  NoticeFieldType 0x71

pattern LineNoticeFieldType =
  NoticeFieldType 0x4C

pattern MessageNoticeFieldType =
  NoticeFieldType 0x4D

pattern PositionNoticeFieldType =
  NoticeFieldType 0x50

pattern RoutineNoticeFieldType =
  NoticeFieldType 0x52

pattern SchemaNoticeFieldType =
  NoticeFieldType 0x73

pattern SeverityNoticeFieldType =
  NoticeFieldType 0x53

pattern TableNoticeFieldType =
  NoticeFieldType 0x74


data Error =
  -- | 
  -- An erroneous result received from the DB.
  -- The components are:
  -- 
  -- * The SQLSTATE code for the error. The SQLSTATE code identifies the type of error that has occurred; 
  -- it can be used by front-end applications to perform specific operations (such as error handling) 
  -- in response to a particular database error. 
  -- For a list of the possible SQLSTATE codes, see Appendix A.
  -- This field is not localizable, and is always present.
  -- 
  -- * The primary human-readable error message (typically one line). Always present.
  Error !ByteString !ByteString


data AuthenticationMessage =
  OkAuthenticationMessage |
  ClearTextPasswordAuthenticationMessage |
  MD5PasswordAuthenticationMessage !ByteString
  deriving (Show, Eq)


data NotificationMessage =
  NotificationMessage !Word32 !ByteString !ByteString
