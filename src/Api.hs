{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Api where

import           Data.Aeson       (FromJSON (..), Options (..), ToJSON (..),
                                   genericParseJSON, genericToJSON, object,
                                   (.=))
import           Data.Aeson.Types (defaultOptions)
import           Data.Text        (Text)
import           Data.Time        (UTCTime)
import           GHC.Generics     (Generic)
import           Servant          (Capture, Get, JSON, Post, ReqBody, type (:>))
import           Servant.API      (GenericMode (type (:-)))

type MessageId = Word

newtype IdObj = IdObj { id_ :: MessageId }
  deriving (Show, Generic)

instance ToJSON IdObj where
  toJSON = genericToJSON jsonOptions

data MessageOut = MessageOut
  { id_       :: !MessageId
  , message   :: !Text
  , tags      :: ![Text]
  , createdAt :: !UTCTime
  } deriving (Show, Generic)

data MessageIn = MessageIn
  { message :: !Text
  , tags    :: ![Text]
  } deriving (Show, Generic)

data Ok = Ok deriving Show

jsonOptions :: Options
jsonOptions = defaultOptions
  { fieldLabelModifier = \case
      "id_" -> "id"
      s     -> s
  }

instance ToJSON MessageOut where
  toJSON = genericToJSON jsonOptions

instance FromJSON MessageIn where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Ok where
  toJSON Ok = object ["status" .= ("ok" :: Text)]

data Api routes = Api
  { getMessage :: routes :- "get" :> "message" :> Capture "messageId" MessageId :> Get '[JSON] MessageOut
  , listTag    :: routes :- "list" :> "tag" :> Capture "tag" Text :> Get '[JSON] [MessageOut]
  , save       :: routes :- "save" :> ReqBody '[JSON] MessageIn :> Post '[JSON] IdObj
  , toggleLogs :: routes :- "toggle-logs" :> Post '[JSON] Ok
  } deriving Generic
