{-# LANGUAGE RankNTypes #-}
module Launch where
import           Control.Monad.Except     (ExceptT (..))
import           CurrentTime              (runCurrentTimeIO)
import qualified Data.Text                as T
import           Effectful                (Limit (..), Persistence (..),
                                           UnliftStrategy (..), liftIO, runEff,
                                           withEffToIO, withUnliftStrategy)
import           Effectful.Concurrent     (runConcurrent)
import           GHC.IO.Encoding          (setForeignEncoding,
                                           setLocaleEncoding, utf8)
import           GHC.IO.Handle            (hSetEncoding)
import           Logger                   (disableLogger, logError, logInfo,
                                           runLoggerState, runStdoutLogger)
import qualified Logger
import           Middlewares              (serverHandlerLoggerMiddleware,
                                           storageLoggerMiddleware)
import           Network.Wai.Handler.Warp (defaultSettings, runSettings,
                                           setOnException, setPort)
import           Servant.Server           (Handler (..))
import           Servant.Server.Generic   (genericServeT)
import           Server                   (apiHandler, runServerHandler)
import           Storage                  (runStorageSTM)
import           System.IO                (stderr, stdin, stdout)

launch :: IO ()
launch = do
  setLocaleEncoding utf8
  setForeignEncoding utf8
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  runEffects
    $ withUnliftStrategy (ConcUnlift Persistent Unlimited)
    $ withEffToIO \effToIO -> effToIO $ runServer $ UnliftIO effToIO
  where
    runEffects
      = runEff
      . runConcurrent
      . runStdoutLogger
      . runLoggerState
      . runCurrentTimeIO
      . runStorageSTM
      . runServerHandler
      . storageLoggerMiddleware
      . serverHandlerLoggerMiddleware
    disableLoggerIfNeed m = do
      enabled <- Logger.getCurrentState
      let
        runLogs
          | enabled   = id
          | otherwise = disableLogger
      runLogs m
    runServer (UnliftIO effToIO) = do
      logInfo "Starting server on 8080 port"
      liftIO $ runSettings settings
        $ genericServeT (Handler . ExceptT . fmap Right . effToIO . disableLoggerIfNeed) apiHandler
      where
        settings
          = setPort 8080
          . setOnException (\_ e ->
              effToIO $ disableLoggerIfNeed $ logError $ T.pack $ show e)
          $ defaultSettings

newtype UnliftIO m = UnliftIO { unliftIO :: forall a . m a -> IO a }
