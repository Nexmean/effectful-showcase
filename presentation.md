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
- минимальное количество переходов по коду
- интерфейсы зависят от данных
- реализации зависят от интерфейсов
### И как нам с этим поможет effectful?
---
#### Меньше кода лишённого доменного смысла

```haskell
-- effectful лишает нас необходимости писать код для склейки отдельных деталей приложения
-- реализации эффектов склеиваются через композицию,
-- далее не требуется никаких дополнительных действий
runDependencies
  = runEff
  . runConcurrent
  . runLogger
  . runCurrentTimeIO
  . runStorage

-- интерфейсы и реализации не требуют много "безсмысленного" кода
data CurrentTime :: Effect where
  GetCurrentTime :: CurrentTime m UTCTime
makeEffect ''CurrentTime

runCurrentTimeIO :: IOE :> es => Eff (CurrentTime : es) a -> Eff es a
runCurrentTimeIO = interpret \_ -> \case
  GetCurrentTime -> liftIO Time.getCurrentTime
```

---
#### Сложный код скрыт за понятными интерфейсами
```haskell
-- effectful скрывает от нас работу с зависимостями за простым интерфейсом

-- | интерпретирует эффект
interpret
  :: EffectHandler e es -> Eff (e : es) a -> Eff es a

-- | то же самое, но даёт задать приватные зависимости
reinterpret
  :: (Eff privateEs a -> Eff es b) -> EffectHandler e privateEs -> Eff (e : es) a -> Eff es b

-- | заменяет эффект новым,
--   при этом из нового хэндлера доступен вызов старого
interpose
  :: e :> es => EffectHandler e es -> Eff es a -> Eff es a 
                  
-- | то же самое, но с приватными зависимостями
impose
  :: e :> es => (Eff privateEs a -> Eff es b) -> EffectHandler e privateEs -> Eff es a -> Eff es b
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
#### Минимальное количество переходов по коду
```haskell
-- благодаря простой композиции различных реализаций
-- мы легко можем сложить её определение для процесса/хэндлера
-- в одном месте, что значит, что переход от реализации к реализации
-- его зависимости требует всего двух переходов даже без HLS
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

--- 
#### Но есть и ограничения
- нельзя сделать `NonDet` (аналог `ListT`), эффект выражающий возможность метода разветвлять последующией вычисления
- нельзя сделать `Coroutine`, эффект выражающий возможность вычисления прерваться на каком-то месте, а затем продолжить работу с этого места (`yield`, `resume`)
- анлифтинг требует явного указания, если функция `unlift` будет вызываться из другого треда

---

- Первые две проблемы решаются трансформерами `ListT`, `ContT`, как и в случае с ReaderT-паттерном.
- Передача в другой тред чего-то кроме данных или чистых функций в любом случае требует внимательности.
- Сама библиотека при анлифтинге использует fail-fast подход, благодаря чему при вызове `unlift` из неуполномоченного треда мы сразу получим ошибку
