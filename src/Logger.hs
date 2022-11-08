{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Logger where

import           Data.Aeson                 ((.=))
import           Data.Aeson.Encoding        (encodingToLazyByteString, pairs)
import           Data.ByteString.Lazy       as BSL
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time
import           Effectful                  (Eff, Effect, IOE, liftIO,
                                             type (:>))
import           Effectful.Dispatch.Dynamic (interpret, localSeqUnlift,
                                             reinterpret)
import           Effectful.Reader.Static    (ask, local, runReader)
import           Effectful.TH               (makeEffect)

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
        $  "@timestamp" .= show now
        <> "level" .= showLevel level
        <> "namespace" .= T.intercalate " " namespace
        <> "message" .= message
    liftIO $ BSL.putStr $ BSL.append jsonbs "\n"
  WithNamespace ns m -> localSeqUnlift env \unlift ->
    local (<> [ns]) $ unlift m

runNoLogger :: Eff (Logger : es) a -> Eff es a
runNoLogger = interpret \env -> \case
  LogMessage _ _    -> pure ()
  WithNamespace _ m -> localSeqUnlift env \unlift -> unlift m
