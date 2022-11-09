{-# LANGUAGE RecordWildCards #-}
module Server where

import           Api                    (Api (..), IdObj, MessageId)
import qualified Api
import           Control.Exception      (Exception, throwIO)
import           CurrentTime            (CurrentTime, getCurrentTime)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Effectful              (Eff, IOE, liftIO, type (:>))
import           Logger                 (Logger, LoggerState, logInfo,
                                         withNamespace)
import qualified Logger
import           Servant.Server.Generic (AsServerT)
import qualified Storage
import           Storage                (Storage)

apiHandler
  :: (IOE :> es, Logger :> es, LoggerState :> es, CurrentTime :> es, Storage :> es)
  => Api (AsServerT (Eff es))
apiHandler = Api
  { getMessage = getMessageHandler
  , listTag = listTagHandler
  , save = saveHandler
  , toggleLogs = toggleLogsHandler
  }

getMessageHandler
  :: (IOE :> es, Storage :> es, Logger :> es)
  => Api.MessageId -> Eff es Api.MessageOut
getMessageHandler messageId = withNamespace "getMessage" do
  logInfo $ "Handle get/message/" <> T.pack (show messageId) <> " request"
  message <- Storage.getMessage messageId >>= \case
    Left e  -> throwEff e
    Right m -> pure m
  pure $ storageMessageToApiMessageOut messageId message

listTagHandler
  :: (Storage :> es, Logger :> es)
  => Text -> Eff es [Api.MessageOut]
listTagHandler tag = withNamespace "listTag" do
  logInfo $ "Handle list/tag/" <> tag <> " request"
  messagesWIds <- Storage.getMessagesByTag tag
  pure $ map (uncurry storageMessageToApiMessageOut) messagesWIds

saveHandler
  :: (Storage :> es, Logger :> es, CurrentTime :> es)
  => Api.MessageIn -> Eff es IdObj
saveHandler Api.MessageIn{..} = withNamespace "save" do
  logInfo "Handle save request"
  now <- getCurrentTime
  messageId <- Storage.insertMessage Storage.Message
    { createdAt = now
    , .. }
  pure $ Api.IdObj messageId

toggleLogsHandler
  :: (LoggerState :> es, Logger :> es)
  => Eff es Api.Ok
toggleLogsHandler = withNamespace "toggleLogs" do
  logInfo "Handle toggle-logs request"
  Logger.toggleState
  pure Api.Ok

storageMessageToApiMessageOut :: MessageId -> Storage.Message -> Api.MessageOut
storageMessageToApiMessageOut id_ Storage.Message{..} = Api.MessageOut{..}

throwEff :: (IOE :> es, Exception e) => e -> Eff es a
throwEff = liftIO . throwIO
