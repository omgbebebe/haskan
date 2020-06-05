module Graphics.Haskan.Engine where

-- base
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (MVar, isEmptyMVar, newEmptyMVar, newMVar, takeMVar, tryReadMVar, putMVar, withMVar)
import Control.Monad (when, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (isJust)
import Data.Traversable (for)

-- async
import qualified Control.Concurrent.Async as Async

-- clock
import System.Clock (Clock(..), getTime, toNanoSecs)

-- linear
import Linear (V3(..))

-- stm
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TChan (TChan)
import qualified Control.Concurrent.STM.TChan as TChan

-- haskan
--import Graphics.Haskan.Events (EventsQueue)
import Graphics.Haskan.Vulkan.Types (RenderContext)
import Graphics.Haskan.Logger (logI, showT)

data HaskanConfig =
  HaskanConfig{ targetRenderFPS :: !Integer
              , targetPhysicsFPS :: !Integer
              , targetNetworkFPS :: !Integer
              }
  deriving (Show)

data FrameTime =
  FrameTime{ lastTime :: !Integer
           , currentTime :: !Integer
           , deltaTime :: !Integer
           } deriving (Show)

type Position = V3 Float

data Camera =
  Camera { camPos :: Position
         , camLookAt :: Position
         }

data WorldState =
  WorldState { activeCamera :: TVar Camera
             }

data GameState =
  GameState { world :: TVar WorldState
            , isRunning :: TVar Bool
            }

data ControlMessage
  = Terminate

--mainLoop :: MonadIO m => EventsQueue -> RenderContext -> m ()
--mainLoop eventsQueue ctx@RenderContext{..} = do
--mainLoop :: MonadIO m => HaskanConfig -> RenderContext -> m Bool
mainLoop :: MonadIO m => HaskanConfig -> m Bool
mainLoop config@HaskanConfig{..} = liftIO $ do
  putStrLn "starting mainLoop"
  camera <- STM.newTVarIO (Camera (V3 0.0 0.0 (-5.0)) (V3 0.0 0.0 0.0))
  isRunning <- STM.newTVarIO False

  controlChannel <- TChan.newBroadcastTChanIO :: IO (TChan ControlMessage)
  worldState <- STM.newTVarIO (WorldState camera)
 
  printMVar <- newMVar ()
  let gameState = GameState worldState isRunning

  timeNow <- toNanoSecs <$> getTime Monotonic

  renderLoopFinished <- newEmptyMVar
  forkIO (renderLoop targetRenderFPS gameState renderLoopFinished controlChannel)

  physicsLoopFinished <- newEmptyMVar
  forkIO (physicsLoop targetPhysicsFPS gameState physicsLoopFinished controlChannel)

  Async.forConcurrently_ [renderLoopFinished, physicsLoopFinished] $ \sem -> do
    takeMVar sem
    logI "sending Terminate message"
    STM.atomically $ TChan.writeTChan controlChannel Terminate
{-
  let
    waitForFinished = do
--      logI "waiting all threads to finish"
      sems <- for [renderLoopFinished, physicsLoopFinished] $ \semaphore ->
        isEmptyMVar semaphore
      when (any not sems) $ do
        logI "sending Terminate message"
        STM.atomically $ TChan.writeTChan controlChannel Terminate
      threadDelay (10^4)
      unless (all not sems) waitForFinished

  waitForFinished
-}
  logI "mainLoop finished"
  pure True

--renderLoop :: MonadIO m => Integer -> RenderContext -> GameState -> MVar () -> TChan ControlMessage -> (String -> IO ()) -> m ()
renderLoop :: MonadIO m => Integer -> GameState -> MVar () -> TChan ControlMessage -> m ()
renderLoop targetFPS gameState finishedSemaphore controlChannel = liftIO $ do
  control <- STM.atomically $ TChan.dupTChan controlChannel

  let
    loop :: MonadIO m => Integer -> GameState -> MVar () -> m ()
    loop targetFPS gameState finishedSemaphore = liftIO $ do
      maybeControlMessage <- STM.atomically $ TChan.tryReadTChan control
      case maybeControlMessage of
        Nothing -> do
          worldState <- STM.readTVarIO (world gameState)
          camera <- STM.readTVarIO (activeCamera worldState)

          logI ("render frame: " <> showT targetFPS <> " => " <> showT (camPos camera))
          threadDelay (10^5)
          when (targetFPS > 0) $ loop (targetFPS-1) gameState finishedSemaphore
        Just Terminate -> do
          logI "terminating render loop by signal"
        Just _ -> do
          fail "unknown control message"

  loop targetFPS gameState finishedSemaphore
  logI "renderLoop finished"
  putMVar finishedSemaphore ()


--physicsLoop :: MonadIO m => Integer -> RenderContext -> GameState -> MVar () -> TChan ControlMessage -> (String -> IO ()) -> m ()
physicsLoop :: MonadIO m => Integer -> GameState -> MVar () -> TChan ControlMessage -> m ()
physicsLoop targetFPS gameState finishedSemaphore controlChannel = liftIO $ do
  control <- STM.atomically $ TChan.dupTChan controlChannel

  let
    loop :: MonadIO m => Integer -> GameState -> MVar () -> m ()
    loop targetFPS gameState finishedSemaphore = liftIO $ do
      maybeControlMessage <- STM.atomically $ TChan.tryReadTChan control
      case maybeControlMessage of
        Nothing -> do
          worldState <- STM.readTVarIO (world gameState)
          STM.atomically . STM.modifyTVar (activeCamera worldState) $ \camera ->
            camera{ camPos = (camPos camera) - (V3 0.0 0.0 0.1) }
          logI ("physics frame: " <> showT targetFPS <> " => camPos modified")
          threadDelay (10^5)
          when (targetFPS > 0) $ loop (targetFPS-2) gameState finishedSemaphore
        Just Terminate -> do
          logI "terminating physics loop by signal"
        Just _ -> do
          fail "unknown control message"

  loop targetFPS gameState finishedSemaphore
  logI "physicsLoop finished"
  putMVar finishedSemaphore ()
