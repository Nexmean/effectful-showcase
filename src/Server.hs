{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE RecordWildCards    #-}
module Server where

import           Api                        (Api (Api), MessageId)
import qualified Api
import           Control.Exception          (Exception)
import           CurrentTime                (CurrentTime, getCurrentTime)
import           Data.Text                  (Text)
import           Effectful                  (Eff, Effect, type (:>))
import           Effectful.Dispatch.Dynamic (reinterpret)
import           Effectful.Error.Static     (Error, runErrorNoCallStack,
                                             throwError)
import           Effectful.TH               (makeEffect)
import           Logger                     (LoggerState)
import qualified Logger
import           Servant.Server.Generic     (AsServerT)
import qualified Storage
import           Storage                    (Storage, StorageError)

data Server :: Effect where
  GetMessage :: Api.MessageId -> Server m Api.MessageOut
  ListTag :: Text -> Server m [Api.MessageOut]
  Save :: Api.MessageIn -> Server m Api.IdObj
  ToggleLogs :: Server m Api.Ok

makeEffect ''Server

apiHandler
  :: Server :> es
  => Api (AsServerT (Eff es))
apiHandler = Api{ getMessage, listTag, save, toggleLogs }

newtype ServerError = StorageError { storageError :: StorageError }
  deriving stock Show
  deriving anyclass Exception

runServerHandler
  :: ( LoggerState :> es
     , Storage :> es
     , CurrentTime :> es )
  => Eff (Server : es) a
  -> Eff es (Either ServerError a)
runServerHandler = reinterpret (runErrorNoCallStack @ServerError) \_ -> \case
  GetMessage mId -> getMessageHandler mId
  ListTag tag    -> listTagHandler tag
  Save mIn       -> saveHandler mIn
  ToggleLogs     -> toggleLogsHandler

getMessageHandler
  :: (Error ServerError :> es, Storage :> es)
  => Api.MessageId -> Eff es Api.MessageOut
getMessageHandler messageId = do
  message <- Storage.getMessage messageId >>= throwEitherMap StorageError
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

throwEitherMap :: Error err' :> es => (err -> err') -> Either err a -> Eff es a
throwEitherMap f = \case
  Left e    -> throwError $ f e
  Right res -> pure res
