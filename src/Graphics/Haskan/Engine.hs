module Graphics.Haskan.Engine where

-- base
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Monad (when, replicateM, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Foreign.C

-- async
import qualified Control.Concurrent.Async as Async

-- clock
-- import System.Clock (Clock(..), getTime, toNanoSecs)

-- lens
import Control.Lens ((&), (.~))

-- linear
import Linear (V2(..), V3(..), M44, fromQuaternion)
import Linear.Matrix ((!*!), identity, m33_to_m44, translation)
import qualified Linear.Matrix
import qualified Linear.Projection
import qualified Linear.Quaternion

-- managed
import Control.Monad.Managed (MonadManaged, with, runManaged)

-- sdl2
import qualified SDL

-- stm
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TChan (TChan)
import qualified Control.Concurrent.STM.TChan as TChan

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Ext as Vulkan

-- haskan
import qualified Graphics.Haskan.Events as Events
import           Graphics.Haskan.Logger (logI, showT)
import           Graphics.Haskan.Resources (throwVkResult)
import           Graphics.Haskan.Vulkan.Render (drawFrame, presentFrame)
import qualified Graphics.Haskan.Vulkan.Buffer as Buffer
import qualified Graphics.Haskan.Vulkan.Render as Render
import qualified Graphics.Haskan.Vulkan.CommandPool as CommandPool
import qualified Graphics.Haskan.Vulkan.DescriptorPool as DescriptorPool
import qualified Graphics.Haskan.Vulkan.DescriptorSet as DescriptorSet
import qualified Graphics.Haskan.Vulkan.DescriptorSetLayout as DescriptorSetLayout
import qualified Graphics.Haskan.Vulkan.Device as Device
import qualified Graphics.Haskan.Vulkan.Fence as Fence
import qualified Graphics.Haskan.Vulkan.Instance as Instance
import qualified Graphics.Haskan.Vulkan.PipelineLayout as PipelineLayout
import qualified Graphics.Haskan.Vulkan.PhysicalDevice as PhysicalDevice
import qualified Graphics.Haskan.Vulkan.Semaphore as Semaphore
import qualified Graphics.Haskan.Vulkan.ShaderModule as ShaderModule
import           Graphics.Haskan.Vulkan.Types (RenderContext(..))
import qualified Graphics.Haskan.Window as Window

data EngineConfig =
  EngineConfig{ targetRenderFPS :: !Integer
              , targetPhysicsFPS :: !Integer
              , targetNetworkFPS :: !Integer
              , targetInputFPS :: !Integer
              , title :: !Text
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
mainLoop :: MonadIO m => EngineConfig -> m ()
mainLoop EngineConfig{..} = do
  logI "starting mainLoop"
  camera <- liftIO $ STM.newTVarIO (Camera (V3 0.0 0.0 (-5.0)) (V3 0.0 0.0 0.0))
  isRunning <- liftIO $ STM.newTVarIO False

  controlChannel <- liftIO $ TChan.newBroadcastTChanIO
  worldState <- liftIO $ STM.newTVarIO (WorldState camera)
 
  let gameState = GameState worldState isRunning

  -- timeNow <- liftIO $ toNanoSecs <$> getTime Monotonic

  renderLoopFinished <- liftIO $ newEmptyMVar
  _ <- liftIO $ forkIO (runManaged (renderLoop title targetRenderFPS gameState renderLoopFinished controlChannel))

  physicsLoopFinished <- liftIO $ newEmptyMVar
  _ <- liftIO $ forkIO (physicsLoop targetPhysicsFPS gameState physicsLoopFinished controlChannel)

  inputLoopFinished <- liftIO $ newEmptyMVar
  _ <- liftIO $ forkIO (runManaged $ inputLoop targetInputFPS inputLoopFinished controlChannel)

  liftIO $ Async.forConcurrently_ [renderLoopFinished, physicsLoopFinished, inputLoopFinished] $ \sem -> do
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

renderFrameLoop
  :: (MonadFail m, MonadIO m)
  => RenderContext
  -> Int
  -> [Vulkan.VkSemaphore]
  -> TChan ControlMessage
  -> Vulkan.VkDeviceMemory
  -> TVar Camera
  -> m Bool
renderFrameLoop ctx@RenderContext{..} frameNumber imageAvailableSemaphores control mvpMemory tvCamera = do
  _events <- SDL.pollEvents
  maybeControlMessage <- liftIO $ STM.atomically $ TChan.tryReadTChan control
  (needRestart, terminating) <- case maybeControlMessage of
    Nothing -> do
      let imageAvailableSemaphore = imageAvailableSemaphores !! (frameNumber)
      camera <- liftIO $ STM.readTVarIO tvCamera
      let newMVP = mvpMatrix (coerce (camPos camera)) (coerce (camLookAt camera))
      Buffer.updateUniformBuffer device mvpMemory newMVP
      res <- liftIO $ drawFrame ctx imageAvailableSemaphore frameNumber
      case res of
        Render.FrameOk imageIndex -> do
          presentResult <- liftIO $ presentFrame ctx imageIndex (renderFinishedSemaphores !! (fromIntegral imageIndex))
          case presentResult of
            Vulkan.VK_SUCCESS -> pure (False, False)
            Vulkan.VK_SUBOPTIMAL_KHR -> pure (True, False)
            Vulkan.VK_ERROR_OUT_OF_DATE_KHR -> pure (True, False)
            _ -> fail "presentFrame failed"
        Render.FrameSuboptimal _ -> do
          fail "suboptimal"
        Render.FrameOutOfDate -> do
          logI "resizing swapchain"
          pure (True, False)
        Render.FrameFailed err -> fail err
    Just Terminate -> do
      logI "terminating render loop by signal"
      pure (True, True)

  if (needRestart)
    then liftIO $ do
      logI "waiting IDLE state for device"
      Vulkan.vkDeviceWaitIdle device >>= throwVkResult
      logI "terminating renderFrameLoop"
      pure terminating
    else renderFrameLoop ctx ((frameNumber + 1) `mod` Render.maxFramesInFlight) imageAvailableSemaphores control mvpMemory tvCamera

--renderLoop :: MonadIO m => Integer -> RenderContext -> GameState -> MVar () -> TChan ControlMessage -> (String -> IO ()) -> m ()
renderLoop :: MonadManaged m => Text -> Integer -> GameState -> MVar () -> TChan ControlMessage -> m ()
renderLoop title _targetFPS gameState finishedSemaphore controlChannel = do
  control <- liftIO $ STM.atomically $ TChan.dupTChan controlChannel
  logI "Initializing events managed"

  logI "Initialize base Render context"
  let initWidth = 1920
      initHeight = 1080
  (windowExts, window) <- Window.managedWindow title (initWidth, initHeight)
  (inst, layers)   <- Instance.managedInstance windowExts
  surface          <- Window.managedSurface inst window
  physicalDevice   <- PhysicalDevice.selectPhysicalDevice inst
  Window.showWindow window

  (device, (graphicsQueueFamilyIndex, presentQueueFamilyIndex)) <- Device.managedRenderDevice physicalDevice surface layers

  graphicsQueueHandler <- Device.getDeviceQueueHandler device graphicsQueueFamilyIndex 0
  presentQueueHandler <- Device.getDeviceQueueHandler device presentQueueFamilyIndex 0

  vertShader <- ShaderModule.managedShaderModule device "data/shaders/mvp/vert.spv"
  fragShader <- ShaderModule.managedShaderModule device "data/shaders/mvp/frag.spv"

  descriptorSetLayout <- DescriptorSetLayout.managedDescriptorSetLayout device

  descriptorPool <- DescriptorPool.managedDescriptorPool device 4 -- imageViewCount here
  descriptorSets <- replicateM 4 (DescriptorSet.allocateDescriptorSet device descriptorPool [descriptorSetLayout])

  pipelineLayout <- PipelineLayout.managedPipelineLayout device [descriptorSetLayout]
  graphicsCommandPool <- CommandPool.managedCommandPool device graphicsQueueFamilyIndex

  imageAvailableSemaphores <- replicateM Render.maxFramesInFlight (Semaphore.managedSemaphore device)
  renderFinishedSemaphores <- replicateM 4 (Semaphore.managedSemaphore device)
  renderFinishedFences <- replicateM Render.maxFramesInFlight (Fence.managedFence device)

  let
    zPos1 = ( 5)
    zPos2 = ( 3)
    zPos3 = ( 1)
    vertices =
      [V2 (V3 (-1.0) ( 1.0) zPos1) (V3 1.0 0.0 0.0) -- 0
      ,V2 (V3 (-1.0) (-1.0) zPos1) (V3 1.0 0.0 0.0) -- 1
      ,V2 (V3 ( 1.0) (-1.0) zPos1) (V3 1.0 0.0 0.0) -- 2
      ,V2 (V3 ( 1.0) ( 1.0) zPos1) (V3 1.0 0.0 0.0) -- 3

      ,V2 (V3 (-1.0) ( 1.0) zPos2) (V3 0.0 1.0 0.0) -- 4
      ,V2 (V3 (-1.0) (-1.0) zPos2) (V3 0.0 1.0 0.0) -- 5
      ,V2 (V3 ( 1.0) (-1.0) zPos2) (V3 0.0 1.0 0.0) -- 6
      ,V2 (V3 ( 1.0) ( 1.0) zPos2) (V3 0.0 1.0 0.0) -- 7

      ,V2 (V3 (-1.0) ( 1.0) zPos3) (V3 1.0 1.0 0.0) -- 8
      ,V2 (V3 (-1.0) (-1.0) zPos3) (V3 1.0 1.0 0.0) -- 9
      ,V2 (V3 ( 1.0) (-1.0) zPos3) (V3 1.0 1.0 0.0) -- 10
      ,V2 (V3 ( 1.0) ( 1.0) zPos3) (V3 1.0 1.0 0.0) -- 11
      ]
    indices = [
        0, 1, 2
      , 2, 3, 0

      , 4, 5, 6
      , 6, 7, 4

      , 8, 9, 10
      , 10, 11, 8
              ]

  vertexBuffer <-
    Buffer.managedVertexBuffer
    physicalDevice
    device
    vertices

  indexBuffer <-
    Buffer.managedIndexBuffer
    physicalDevice
    device
    indices


  (mvpBuffer, mvpMemory) <-
    Buffer.managedUniformBuffer
       physicalDevice
       device
       [ mvpMatrix (V3 0.0 0.0 (-5.0)) (V3 0.0 0.0 0.0)]


  for_ descriptorSets $ \descriptorSet -> DescriptorSet.updateDescriptorSets device descriptorSet mvpBuffer

  let
    mkRenderContext = Render.createRenderContext
      physicalDevice
      device
      surface
      pipelineLayout
      vertShader
      fragShader
      descriptorSets
      graphicsCommandPool
      graphicsQueueHandler
      presentQueueHandler
      renderFinishedFences
      renderFinishedSemaphores
      [vertexBuffer]
      [indexBuffer]

  -- need to fetch RenderContext from Managed monad to allow proper resource deallocation
  worldState <- liftIO $ STM.readTVarIO (world gameState)
  let
    tvCamera = activeCamera worldState
    outerLoop :: (MonadFail m, MonadIO m) => Bool -> m ()
    outerLoop exit = do
      if exit
      then pure ()
      else do
        renderFrameLoopFinished <- liftIO $ with mkRenderContext $ \context ->
          renderFrameLoop context 0 imageAvailableSemaphores control mvpMemory tvCamera
        outerLoop renderFrameLoopFinished

  logI "Starting render loop"
  liftIO $ outerLoop False

  logI "renderLoop finished"
  liftIO $ putMVar finishedSemaphore ()

mvpMatrix :: V3 Foreign.C.CFloat -> V3 Foreign.C.CFloat -> M44 Foreign.C.CFloat
mvpMatrix eyePos target =
  let
    view =
      Linear.Projection.lookAt eyePos target (V3 0.0 (-1.0) 0.0)
    model =
      let
        rotate = m33_to_m44 (fromQuaternion (Linear.Quaternion.axisAngle (V3 1.0 1.0 0.0) (pi / 12)))
        translate = identity & translation .~ V3 0 0 (5.0)
      in translate !*! rotate
    projection =
      Linear.Projection.perspective
      (pi / 6) -- FOV
      (16/9) -- aspect ratio
      0.01 -- near plane
      100.0 -- far plane

  in Linear.Matrix.transpose (projection !*! view !*! model)

physicsLoop :: MonadIO m => Integer -> GameState -> MVar () -> TChan ControlMessage -> m ()
physicsLoop targetFPS gameState finishedSemaphore controlChannel = liftIO $ do
  control <- STM.atomically $ TChan.dupTChan controlChannel

  let
    loop :: MonadIO m => Integer -> GameState -> m ()
    loop tFPS _gameState = liftIO $ do
      maybeControlMessage <- STM.atomically $ TChan.tryReadTChan control
      case maybeControlMessage of
        Nothing -> do
          worldState <- STM.readTVarIO (world gameState)
          STM.atomically . STM.modifyTVar (activeCamera worldState) $ \camera ->
            camera{ camPos = (camPos camera) - (V3 0.0 0.0 0.1) }
          logI ("physics frame: " <> showT tFPS <> " => camPos modified")
          threadDelay (100000)
          when (tFPS > 0) $ loop (tFPS-1) _gameState
        Just Terminate -> do
          logI "terminating physics loop by signal"

  loop targetFPS gameState
  logI "physicsLoop finished"
  putMVar finishedSemaphore ()


inputLoop :: MonadManaged m => Integer -> MVar () -> TChan ControlMessage -> m ()
inputLoop targetFPS finishedSemaphore controlChannel = do
  control <- liftIO $ STM.atomically $ TChan.dupTChan controlChannel
  Events.managedEvents

  let
    loop :: MonadIO m => Integer -> m ()
    loop tFPS = liftIO $ do
      maybeControlMessage <- STM.atomically $ TChan.tryReadTChan control
      case maybeControlMessage of
        Nothing -> do
          events <- SDL.pollEvents
          let
            eventIsQPress event =
              case SDL.eventPayload event of
                SDL.KeyboardEvent keyboardEvent ->
                  SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
                  SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
                _ -> False
            qPressed = any eventIsQPress events
          threadDelay (1000)
          when (qPressed) (logI "Q has been pressed")
          unless (qPressed) (loop tFPS)
        Just Terminate -> do
          logI "terminating input loop by signal"

  loop targetFPS
  logI "inputLoop finished"
  liftIO $ putMVar finishedSemaphore ()
