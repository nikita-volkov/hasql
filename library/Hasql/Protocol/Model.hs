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
newtype BackendMessageType =
  BackendMessageType Word8
  deriving (Eq, Ord)

instance Show BackendMessageType where
  show =
    \case
      AuthenticationBackendMessageType -> "AuthenticationBackendMessageType"
      KeyDataBackendMessageType -> "KeyDataBackendMessageType"
      BindCompleteBackendMessageType -> "BindCompleteBackendMessageType"
      CloseCompleteBackendMessageType -> "CloseCompleteBackendMessageType"
      CommandCompleteBackendMessageType -> "CommandCompleteBackendMessageType"
      CopyOutDataBackendMessageType -> "CopyOutDataBackendMessageType"
      CopyOutDoneBackendMessageType -> "CopyOutDoneBackendMessageType"
      CopyInResponseBackendMessageType -> "CopyInResponseBackendMessageType"
      CopyOutBackendMessageType -> "CopyOutBackendMessageType"
      CopyBothBackendMessageType -> "CopyBothBackendMessageType"
      DataRowBackendMessageType -> "DataRowBackendMessageType"
      EmptyQueryBackendMessageType -> "EmptyQueryBackendMessageType"
      ErrorBackendMessageType -> "ErrorBackendMessageType"
      FunctionCallBackendMessageType -> "FunctionCallBackendMessageType"
      NoDataBackendMessageType -> "NoDataBackendMessageType"
      NoticeBackendMessageType -> "NoticeBackendMessageType"
      NotificationBackendMessageType -> "NotificationBackendMessageType"
      ParameterDescriptionBackendMessageType -> "ParameterDescriptionBackendMessageType"
      ParameterStatusBackendMessageType -> "ParameterStatusBackendMessageType"
      ParseCompleteBackendMessageType -> "ParseCompleteBackendMessageType"
      PortalSuspendedBackendMessageType -> "PortalSuspendedBackendMessageType"
      ReadyForQueryBackendMessageType -> "ReadyForQueryBackendMessageType"
      RowDescriptionBackendMessageType -> "RowDescriptionBackendMessageType"
      BackendMessageType x -> "BackendMessageType " <> show x

pattern AuthenticationBackendMessageType =
  BackendMessageType 82

pattern KeyDataBackendMessageType =
  BackendMessageType 75

pattern BindCompleteBackendMessageType =
  BackendMessageType 50

pattern CloseCompleteBackendMessageType =
  BackendMessageType 51

pattern CommandCompleteBackendMessageType =
  BackendMessageType 67

pattern CopyOutDataBackendMessageType =
  BackendMessageType 100

pattern CopyOutDoneBackendMessageType =
  BackendMessageType 99

pattern CopyInResponseBackendMessageType =
  BackendMessageType 71

pattern CopyOutBackendMessageType =
  BackendMessageType 72

pattern CopyBothBackendMessageType =
  BackendMessageType 87

pattern DataRowBackendMessageType =
  BackendMessageType 68

pattern EmptyQueryBackendMessageType =
  BackendMessageType 73

pattern ErrorBackendMessageType =
  BackendMessageType 69

pattern FunctionCallBackendMessageType =
  BackendMessageType 86

pattern NoDataBackendMessageType =
  BackendMessageType 110

pattern NoticeBackendMessageType =
  BackendMessageType 78

pattern NotificationBackendMessageType =
  BackendMessageType 65

pattern ParameterDescriptionBackendMessageType =
  BackendMessageType 116

pattern ParameterStatusBackendMessageType =
  BackendMessageType 83

pattern ParseCompleteBackendMessageType =
  BackendMessageType 49

pattern PortalSuspendedBackendMessageType =
  BackendMessageType 115

pattern ReadyForQueryBackendMessageType =
  BackendMessageType 90

pattern RowDescriptionBackendMessageType =
  BackendMessageType 84


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


data NotificationMessage =
  NotificationMessage !Word32 !ByteString !ByteString
