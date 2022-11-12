module Middlewares where

import qualified Data.Text                  as T
import           Effectful                  (Eff, type (:>))
import           Effectful.Dispatch.Dynamic (interpose)
import           Logger                     (Logger, logError, logInfo,
                                             withNamespace)
import qualified Server
import           Server                     (Server)
import qualified Storage
import           Storage                    (Storage)

storageLoggerMiddleware :: (Storage :> es, Logger :> es) => Eff es a -> Eff es a
storageLoggerMiddleware = interpose \_ -> withNamespace "storage" . \case
  Storage.GetMessage messageId -> do
    logInfo $ "Handling Storage.getMessage with messageId=" <> T.pack (show messageId)
    res <- Storage.getMessage messageId
    case res of
      Left e        -> logError $ T.pack $ show e
      Right message -> logInfo $ "Storage.getMessage respond with " <> T.pack (show message)
    pure res

  Storage.GetMessagesByTag tag -> do
    logInfo $ "Handling getMessagesByTag with tag=" <> tag
    res <- Storage.getMessagesByTag tag
    logInfo $ "getMessagesByTag respond with " <> (T.pack . show . length $ res) <> " messages"
    pure res

  Storage.InsertMessage message -> do
    logInfo $ "Handling inserMessage with message=" <> T.pack (show message)
    res <- Storage.insertMessage message
    logInfo $ "insertMessage respond with " <> T.pack (show res)
    pure res

serverLoggerMiddleware :: (Server :> es, Logger :> es) => Eff es a -> Eff es a
serverLoggerMiddleware = interpose \_ -> withNamespace "server" . \case
  Server.GetMessage messageId -> withNamespace "getMessage" do
    logInfo $ "Handling request with messageId=" <> T.pack (show messageId)
    res <- Server.getMessage messageId
    logInfo $ "Responding with " <> T.pack (show res)
    pure res

  Server.ListTag tag -> withNamespace "listTag" do
    logInfo $ "Handling request with tag=" <> tag
    res <- Server.listTag tag
    logInfo $ "Responding with " <> T.pack (show res)
    pure res

  Server.Save message -> withNamespace "save" do
    logInfo $ "Handling request with message=" <> T.pack (show message)
    res <- Server.save message
    logInfo $ "Responding with " <> T.pack (show res)
    pure  res

  Server.ToggleLogs -> withNamespace "toggleLogs" do
    logInfo "Handling request"
    res <- Server.toggleLogs
    logInfo $ "Responding with " <> T.pack (show res)
    pure res
