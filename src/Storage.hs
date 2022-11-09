{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections       #-}
module Storage
  ( runStorageSTM
  , Storage (..), Message (..), MessageId
  , getMessage, getMessagesByTag, insertMessage
  ) where

import           Control.Exception          (Exception)
import           Data.Foldable              (for_)
import           Data.Functor               ((<&>))
import qualified Data.Map.Strict            as Strict
import qualified Data.Map.Strict            as Strict.Map
import           Data.Maybe                 (maybeToList)
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)
import           DeferredFolds.UnfoldlM     (foldlM')
import           Effectful                  (Eff, Effect, type (:>))
import           Effectful.Concurrent       (Concurrent)
import           Effectful.Concurrent.STM   (STM)
import qualified Effectful.Concurrent.STM   as STM
import           Effectful.Dispatch.Dynamic (interpret)
import           Effectful.TH               (makeEffect)
import qualified StmContainers.Map          as STM (Map)
import qualified StmContainers.Map          as STM.Map
import qualified StmContainers.Set          as STM (Set)
import qualified StmContainers.Set          as STM.Set

type MessageId = Word

newtype StorageError
  = MessageNotFound MessageId
  deriving Show
  deriving anyclass Exception

data Message = Message
  { message   :: !Text
  , tags      :: ![Text]
  , createdAt :: !UTCTime
  }

data Storage :: Effect where
  GetMessage :: MessageId -> Storage m (Either StorageError Message)
  GetMessagesByTag :: Text -> Storage m [(MessageId, Message)]
  InsertMessage :: Message -> Storage m MessageId
makeEffect ''Storage

runStorageSTM :: Concurrent :> es => Eff (Storage : es) a -> Eff es a
runStorageSTM m = do
  storageRepr <- STM.atomically initStorageRepr
  (`interpret` m) \_ -> STM.atomically . \case
    GetMessage messageId  -> getMessageSTM storageRepr messageId <&> \case
      Nothing      -> Left $ MessageNotFound messageId
      Just message -> Right message
    GetMessagesByTag tag  -> getMessagesByTagSTM storageRepr tag
    InsertMessage message -> insertMessageSTM storageRepr message

data StorageRepr = StorageRepr
  { messages      :: !(STM.Map MessageId Message)
  , tagsIndex     :: !(STM.TVar (Strict.Map Text (STM.Set MessageId)))
  , lastMessageId :: !(STM.TVar MessageId)
  }

initStorageRepr :: STM StorageRepr
initStorageRepr = do
  messages <- STM.Map.new
  tagsIndex <- STM.newTVar mempty
  lastMessageId <- STM.newTVar 0
  pure StorageRepr { messages, tagsIndex, lastMessageId }

insertMessageSTM :: StorageRepr -> Message -> STM MessageId
insertMessageSTM storageRepr message' = do
  id_ <- succ <$> STM.readTVar storageRepr.lastMessageId
  STM.writeTVar storageRepr.lastMessageId id_
  STM.Map.insert message' id_ storageRepr.messages
  for_ message'.tags \tag -> do
    index <- STM.readTVar storageRepr.tagsIndex
    (index', ids) <- case Strict.Map.lookup tag index of
      Just ids -> pure (index, ids)
      Nothing  -> do
        ids <- STM.Set.new
        pure (Strict.Map.insert tag ids index, ids)
    STM.Set.insert id_ ids
    STM.writeTVar storageRepr.tagsIndex index'
  pure id_

getMessageSTM :: StorageRepr -> MessageId -> STM (Maybe Message)
getMessageSTM storageRepr id_ = STM.Map.lookup id_ storageRepr.messages

getMessagesByTagSTM :: StorageRepr -> Text -> STM [(MessageId, Message)]
getMessagesByTagSTM storageRepr tag = do
  index <- STM.readTVar storageRepr.tagsIndex
  let
    mMessageIdsSet = Strict.Map.lookup tag index
  case mMessageIdsSet of
    Nothing     -> pure []
    Just idsSet -> foldlM' go [] (STM.Set.unfoldlM idsSet)
      where
        go ms id_ = do
          mMessage <- getMessageSTM storageRepr id_
          pure $ ((id_,) <$> maybeToList mMessage) ++ ms
