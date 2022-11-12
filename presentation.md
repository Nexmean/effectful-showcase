---
theme: gaia
_class: lead
paginate: true
backgroundColor: #fff
---
# Почему effectful
---
## Какими свойствами обладает хороший код?
- его просто читать и понимать
- в нём просто ориентироваться
- в него легко добавлять новое поведение и изменять старое
- его легко тестировать
---
## Как этого достичь?
- меньше кода лишённого доменного смысла
- сложный код скрыт за понятными интерфейсами
- бизнес сценарии отделены от технических деталей
- единая точка определяющая реализацию процесса
- интерфейсы зависят только от типов данных
- реализации зависят от интерфейсов
### И как нам с этим поможет effectful?
---
#### Меньше кода лишённого доменного смысла

```haskell
-- effectful лишает нас необходимости писать код для склейки отдельных деталей приложения
-- реализации эффектов склеиваются через композицию,
-- далее не требуется никакой дополнительной работы
runDependencies
  = runEff
  . runConcurrent
  . runLogger
  . runCurrentTimeIO
  . runStorage

-- реализации так-же не требуют много безсмысленного кода
runStdoutLogger :: IOE :> es => Eff (Logger : es) a -> Eff es a
runStdoutLogger = reinterpret (runReader @[Text] []) \env -> \case
  LogMessage level message -> do
    namespace <- ask
    liftIO $ putStrLn $ "[" <> level <> "] " <> intercalate "." namespace <> ": " <> message
  WithNamespace ns m -> local (<> [ns]) do
    unlocalSeqUnlift env \unlift -> lift m
```

---
#### Сложный код скрыт за понятными интерфейсами
```haskell
-- effectful скрывает от нас не всегда простую работу с зависимостями за простым интерфейсом

-- | интерпретирует эффект
interpret
  :: EffectHandler e es -> Eff (e : es) a -> Eff es a

-- | то же самое, но даёт задать приватные зависимости
reinterpret
  :: (Eff privateEs a -> Eff es b) -> EffectHandler e privateEs -> Eff (e : es) a -> Eff es b

-- | заменяет эффект новым,
--   при этом из нового хэндлера доступен вызов старого
interpose
  :: (e :> es) => EffectHandler e es -> Eff es a -> Eff es a 
                  
-- | то же самое, но с приватными зависимостями
impose
  :: (e :> es) => (Eff privateEs a -> Eff es b) -> EffectHandler e privateEs -> Eff es a -> Eff es b
```

---
#### Бизнес сценарии отделены от технических деталей
```haskell
-- легко обернуть имеющуюся реализацию новым поведением
serverLoggerMiddleware :: (Server :> es, Logger :> es) => Eff es a -> Eff es a
serverLoggerMiddleware = interpose \_ -> \case
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

  ...
```

---
#### Единая точка определяющая реализацию процесса
```haskell
runServerHandler' = runServerHandler . serverLoggerMiddleware

runDependencies
  = runEff
  . runConcurrent
  . runLogger
  . runCurrentTimeIO
  . runStorage

runLogger = runStdoutLogger . runLoggerState

runStorage = runStorageSTM . storageLoggerMiddleware
```

---
#### Интерфейсы зависят только от типов данных
```haskell
-- effectful даёт нам возможность легко создавать интерфейсы
-- принимающие монадические действия,
-- благодаря чему можно быть уверенным, что если интерфейсы и потекут,
-- то по нашей вине, а не из-за ограничений подхода/библиотеки
data Logger :: Effect where
  LogMessage :: Text -> Text -> Logger m ()
  WithNamespace :: Text -> m a -> Logger m a

runStdoutLogger :: IOE :> es => Eff (Logger : es) a -> Eff es a
runStdoutLogger = reinterpret (runReader @[Text] []) \env -> \case
  LogMessage level message -> do
    namespace <- ask
    liftIO $ putStrLn $ "[" <> level <> "] " <> intercalate "." namespace <> ": " <> message
  WithNamespace ns m -> local (<> [ns]) do
    localSeqUnlift env \unlift -> unlift m
```

---
#### Реализации зависят от интерфейсов
```haskell
-- таким образом кривые интерфейсы или реализации завязанные на детали реализации
-- других интерфейсов не получится скинуть на подход или библиотеку
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

```
