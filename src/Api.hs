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
import           Servant          (Get, JSON, Post, QueryParam', ReqBody,
                                   Required, Strict, type (:>))
import           Servant.API      (GenericMode (type (:-)))

type MessageId = Word

newtype IdObj = IdObj { id_ :: MessageId }
  deriving Generic

instance ToJSON IdObj where
  toJSON = genericToJSON jsonOptions

data MessageOut = MessageOut
  { id_       :: !MessageId
  , message   :: !Text
  , tags      :: ![Text]
  , createdAt :: !UTCTime
  } deriving Generic

data MessageIn = MessageIn
  { message :: !Text
  , tags    :: ![Text]
  } deriving Generic

data Ok = Ok

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
  { getMessage :: routes :- "get" :> "message" :> QueryParam' [Required, Strict] "messageId" MessageId :> Get '[JSON] MessageOut
  , listTag    :: routes :- "list" :> "tag" :> QueryParam' [Required, Strict] "tag" Text :> Get '[JSON] [MessageOut]
  , save       :: routes :- "save" :> ReqBody '[JSON] MessageIn :> Post '[JSON] IdObj
  , toggleLogs :: routes :- "toggle-logs" :> Post '[JSON] Ok
  } deriving Generic
