{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module Server where

import           Api                        (Api (Api), MessageId)
import qualified Api
import           Control.Exception          (Exception, throwIO)
import           CurrentTime                (CurrentTime, getCurrentTime)
import           Data.Text                  (Text)
import           Effectful                  (Eff, Effect, IOE, liftIO,
                                             type (:>))
import           Effectful.Dispatch.Dynamic (interpret)
import           Effectful.TH               (makeEffect)
import           Logger                     (LoggerState)
import qualified Logger
import           Servant.Server.Generic     (AsServerT)
import qualified Storage
import           Storage                    (Storage)

data ServerHandler :: Effect where
  GetMessage :: Api.MessageId -> ServerHandler m Api.MessageOut
  ListTag :: Text -> ServerHandler m [Api.MessageOut]
  Save :: Api.MessageIn -> ServerHandler m Api.IdObj
  ToggleLogs :: ServerHandler m Api.Ok

makeEffect ''ServerHandler

apiHandler
  :: ServerHandler :> es
  => Api (AsServerT (Eff es))
apiHandler = Api{ getMessage, listTag, save, toggleLogs }

runServerHandler
  :: ( IOE :> es
     , LoggerState :> es
     , Storage :> es
     , CurrentTime :> es
     )
  => Eff (ServerHandler : es) a
  -> Eff es a
runServerHandler = interpret \_ -> \case
  GetMessage mId -> getMessageHandler mId
  ListTag tag    -> listTagHandler tag
  Save mIn       -> saveHandler mIn
  ToggleLogs     -> toggleLogsHandler

getMessageHandler
  :: (IOE :> es, Storage :> es)
  => Api.MessageId -> Eff es Api.MessageOut
getMessageHandler messageId = do
  message <- Storage.getMessage messageId >>= \case
    Left e  -> throwEff e
    Right m -> pure m
  pure $ storageMessageToApiMessageOut messageId message

listTagHandler
  :: Storage :> es
  => Text -> Eff es [Api.MessageOut]
listTagHandler tag = do
  messagesWIds <- Storage.getMessagesByTag tag
  pure $ map (uncurry storageMessageToApiMessageOut) messagesWIds

saveHandler
  :: (Storage :> es, CurrentTime :> es)
  => Api.MessageIn -> Eff es Api.IdObj
saveHandler Api.MessageIn{..} = do
  now <- getCurrentTime
  messageId <- Storage.insertMessage Storage.Message
    { createdAt = now
    , .. }
  pure $ Api.IdObj messageId

toggleLogsHandler
  :: LoggerState :> es
  => Eff es Api.Ok
toggleLogsHandler = do
  Logger.toggleState
  pure Api.Ok

storageMessageToApiMessageOut :: MessageId -> Storage.Message -> Api.MessageOut
storageMessageToApiMessageOut id_ Storage.Message{..} = Api.MessageOut{..}

throwEff :: (IOE :> es, Exception e) => e -> Eff es a
throwEff = liftIO . throwIO
