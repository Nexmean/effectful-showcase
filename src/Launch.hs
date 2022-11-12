{-# LANGUAGE RankNTypes #-}
module Launch where
import           Control.Monad.Except     (ExceptT (..))
import           CurrentTime              (runCurrentTimeIO)
import           Data.Bifunctor           (Bifunctor (first))
import           Effectful                (Limit (..), Persistence (..),
                                           UnliftStrategy (..), liftIO, runEff,
                                           withEffToIO, withUnliftStrategy)
import           Effectful.Concurrent     (runConcurrent)
import           GHC.IO.Encoding          (setForeignEncoding,
                                           setLocaleEncoding, utf8)
import           GHC.IO.Handle            (hSetEncoding)
import           Logger                   (logInfo, runLoggerState,
                                           runStdoutLogger)
import           Middlewares              (serverLoggerMiddleware,
                                           storageLoggerMiddleware)
import           Network.Wai.Handler.Warp (defaultSettings, runSettings,
                                           setPort)
import qualified Servant.Server           as Servant
import           Servant.Server.Generic   (genericServeT)
import           Server                   (ServerError (..), apiHandler,
                                           runServerHandler)
import           Storage                  (StorageError (..), runStorageSTM)
import           System.IO                (stderr, stdin, stdout)

launch :: IO ()
launch = do
  setLocaleEncoding utf8
  setForeignEncoding utf8
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  runDependencies
    $ withUnliftStrategy (ConcUnlift Ephemeral Unlimited)
    $ withEffToIO \effToIO -> effToIO $ runServer $ UnliftIO effToIO
  where
    runServer (UnliftIO effToIO) = do
      logInfo "Starting server on 8080 port"
      liftIO
        $ runSettings (setPort 8080 defaultSettings)
        $ genericServeT
            (servantHandler . fmap (first adaptError) . effToIO . runServerHandler')
            apiHandler

    runServerHandler'
      = runServerHandler
      . serverLoggerMiddleware

    runDependencies
      = runEff
      . runConcurrent
      . runLogger
      . runCurrentTimeIO
      . runStorage

    runLogger
      = runStdoutLogger
      . runLoggerState

    runStorage
      = runStorageSTM
      . storageLoggerMiddleware

    servantHandler = Servant.Handler . ExceptT

    adaptError (StorageError (MessageNotFound _)) = Servant.err404

newtype UnliftIO m = UnliftIO { unliftIO :: forall a . m a -> IO a }
