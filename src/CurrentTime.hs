module CurrentTime where

import           Data.Time                  (UTCTime)
import qualified Data.Time                  as Time
import           Effectful                  (Eff, Effect, IOE, liftIO,
                                             type (:>))
import           Effectful.Dispatch.Dynamic (interpret)
import           Effectful.TH               (makeEffect)

data CurrentTime :: Effect where
  GetCurrentTime :: CurrentTime m UTCTime
makeEffect ''CurrentTime

runCurrentTimeIO :: IOE :> es => Eff (CurrentTime : es) a -> Eff es a
runCurrentTimeIO = interpret \_ GetCurrentTime -> liftIO Time.getCurrentTime

runCurrentTimeMocked :: UTCTime -> Eff (CurrentTime : es) a -> Eff es a
runCurrentTimeMocked mockTime = interpret \_ GetCurrentTime -> pure mockTime
