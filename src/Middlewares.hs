module Middlewares where

import qualified Data.Text                  as T
import           Effectful                  (Eff, type (:>))
import           Effectful.Dispatch.Dynamic (interpose)
import           Logger                     (Logger, logError, logInfo,
                                             withNamespace)
import qualified Storage
import           Storage                    (Storage (..))

storageLoggerMiddleware :: (Storage :> es, Logger :> es) => Eff es a -> Eff es a
storageLoggerMiddleware = interpose \_ -> \case
  GetMessage messageId -> withNamespace "storage" do
    logInfo $ "Handling Storage.getMessage with messageId=" <> T.pack (show messageId)
    res <- Storage.getMessage messageId
    case res of
      Left e        -> logError $ T.pack $ show e
      Right message -> logInfo $ "Storage.getMessage respond with " <> T.pack (show message)
    pure res
  GetMessagesByTag tag -> withNamespace "storage" do
    logInfo $ "Handling getMessagesByTag with tag=" <> tag
    res <- Storage.getMessagesByTag tag
    logInfo $ "getMessagesByTag respond with " <> (T.pack . show . length $ res) <> " messages"
    pure res
  InsertMessage message -> withNamespace "storage" do
    logInfo $ "Handling inserMessage with message=" <> T.pack (show message)
    res <- Storage.insertMessage message
    logInfo $ "insertMessage respond with " <> T.pack (show res)
    pure res
