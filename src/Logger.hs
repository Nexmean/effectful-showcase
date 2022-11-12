module Logger where

import           Control.Monad                 (when)
import           Data.Aeson                    ((.=))
import           Data.Aeson.Encoding           (encodingToLazyByteString, pairs)
import qualified Data.ByteString.Lazy          as BSL
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time                     (getCurrentTime)
import           Effectful                     (Eff, Effect, IOE, liftIO,
                                                type (:>))
import           Effectful.Dispatch.Dynamic    (interpose, localSeqUnlift,
                                                reinterpret)
import           Effectful.Reader.Static       (ask, local, runReader)
import           Effectful.State.Static.Shared (evalState, get, modify)
import           Effectful.TH                  (makeEffect)

data LogLevel
  = LogInfo
  | LogError

showLevel :: LogLevel -> Text
showLevel = \case
  LogInfo  -> "info"
  LogError -> "error"

data Logger :: Effect where
  LogMessage :: LogLevel -> Text -> Logger m ()
  WithNamespace :: Text -> m a -> Logger m a

makeEffect ''Logger

data LoggerState :: Effect where
  GetCurrentState :: LoggerState m Bool
  ToggleState :: LoggerState m ()

makeEffect ''LoggerState

logInfo, logError :: Logger :> es => Text -> Eff es ()
logInfo = logMessage LogInfo
logError = logMessage LogError

runStdoutLogger :: IOE :> es => Eff (Logger : es) a -> Eff es a
runStdoutLogger = reinterpret (runReader @[Text] []) \env -> \case
  LogMessage level message -> do
    namespace <- ask
    now <- liftIO getCurrentTime
    let
      jsonbs = encodingToLazyByteString $ pairs
        $  "time" .= show now
        <> "level" .= showLevel level
        <> "namespace" .= T.intercalate " " namespace
        <> "message" .= message
    liftIO $ BSL.putStr $ BSL.append jsonbs "\n"
  WithNamespace ns m -> local (<> [ns]) do
    localSeqUnlift env \unlift -> unlift m

runLoggerState :: Logger :> es => Eff (LoggerState : es) a -> Eff es a
runLoggerState = runState' . augmentLogger
  where
    runState' = reinterpret (evalState @Bool True) \_ -> \case
      GetCurrentState -> get
      ToggleState     -> modify not
    augmentLogger = interpose \env -> \case
      LogMessage lvl msg -> do
        enabled <- getCurrentState
        when enabled $ logMessage lvl msg
      WithNamespace ns m -> withNamespace ns do
        localSeqUnlift env \unlift -> unlift m
