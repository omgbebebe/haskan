{-# language DeriveGeneric #-}
module Graphics.Haskan.Engine where

-- base
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Monad (when, replicateM, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Foreign.C
import GHC.Generics
import Debug.Trace

-- async
import qualified Control.Concurrent.Async as Async

-- clock
import System.Clock (Clock(..), getTime, toNanoSecs)

-- hashable
import Data.Hashable (Hashable(..))

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
import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TChan (TChan)
import qualified Control.Concurrent.STM.TChan as TChan
import Control.Concurrent.STM.TQueue (TQueue)
import qualified Control.Concurrent.STM.TQueue as TQueue

-- unordered-containers
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

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

data Action
  = MoveForward
  | MoveBackward
  | StrafeLeft
  | StrafeRight
  | MouseMove (V2 Int)
  | Escape
  deriving (Show)

type ActionEvent = (Action, Bool)

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
            , moveForward :: TVar Bool
            , moveBackward :: TVar Bool
            , strafeLeft :: TVar Bool
            , strafeRight :: TVar Bool
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
  isRunning <- liftIO $ STM.newTVarIO True

  controlChannel <- liftIO $ TChan.newBroadcastTChanIO
  worldState <- liftIO $ STM.newTVarIO (WorldState camera)
  actionQueue <- liftIO $ STM.newTQueueIO
  -- movement state
  tvMoveForward <- liftIO $ STM.newTVarIO (False)
  tvMoveBackward <- liftIO $ STM.newTVarIO (False)
  tvStrafeLeft <- liftIO $ STM.newTVarIO (False)
  tvStrafeRight <- liftIO $ STM.newTVarIO (False)
 
  let
    gameState =
      GameState
        worldState
        isRunning
        tvMoveForward
        tvMoveBackward
        tvStrafeLeft
        tvStrafeRight

  -- timeNow <- liftIO $ toNanoSecs <$> getTime Monotonic

  renderLoopFinished <- liftIO $ newEmptyMVar
  _ <- liftIO $ forkIO (runManaged (renderLoop title targetRenderFPS gameState renderLoopFinished controlChannel))

  stateUpdateLoopFinished <- liftIO $ newEmptyMVar
  _ <- liftIO $ forkIO (stateUpdateLoop targetPhysicsFPS gameState stateUpdateLoopFinished actionQueue controlChannel)

  inputLoopFinished <- liftIO $ newEmptyMVar
  _ <- liftIO $ forkIO (runManaged $ inputLoop targetInputFPS inputLoopFinished actionQueue controlChannel)

  liftIO $ Async.forConcurrently_ [renderLoopFinished, stateUpdateLoopFinished, inputLoopFinished] $ \sem -> do
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
  -> Integer
  -> [Vulkan.VkSemaphore]
  -> TChan ControlMessage
  -> Vulkan.VkDeviceMemory
  -> TVar Camera
  -> m Bool
renderFrameLoop ctx@RenderContext{..} frameNumber targetFPS imageAvailableSemaphores control mvpMemory tvCamera = do
  frameStartTime <- liftIO $ toNanoSecs <$> getTime Monotonic
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

  frameEndTime <- liftIO $ toNanoSecs <$> getTime Monotonic
  if (needRestart)
    then liftIO $ do
      logI "waiting IDLE state for device"
      Vulkan.vkDeviceWaitIdle device >>= throwVkResult
      logI "terminating renderFrameLoop"
      pure terminating
    else do
      let
        renderTime = frameEndTime - frameStartTime
        delay = ((1000000000 `div` targetFPS) - renderTime) `div` 1000
      liftIO $ threadDelay (fromIntegral delay)
      -- logI $ "renderTime: " <> showT renderTime <> " => delay: " <> showT delay
      renderFrameLoop
        ctx
        ((frameNumber + 1) `mod` Render.maxFramesInFlight)
        targetFPS
        imageAvailableSemaphores
        control
        mvpMemory
        tvCamera

--renderLoop :: MonadIO m => Integer -> RenderContext -> GameState -> MVar () -> TChan ControlMessage -> (String -> IO ()) -> m ()
renderLoop :: MonadManaged m => Text -> Integer -> GameState -> MVar () -> TChan ControlMessage -> m ()
renderLoop title targetFPS gameState finishedSemaphore controlChannel = do
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
          renderFrameLoop context 0 targetFPS imageAvailableSemaphores control mvpMemory tvCamera
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

stateUpdateLoop :: MonadIO m => Integer -> GameState -> MVar () -> TQueue ActionEvent -> TChan ControlMessage -> m ()
stateUpdateLoop targetFPS gameState finishedSemaphore actionQueue controlChannel = liftIO $ do
  control <- STM.atomically $ TChan.dupTChan controlChannel

  let physicsStep = 1/120
      frameDelay = physicsStep * 1000000
      camSpeed = 100

  let
    loop :: MonadIO m => Integer -> GameState -> Integer -> m ()
    loop tFPS _gameState prevTime = liftIO $ do
      maybeControlMessage <- STM.atomically $ TChan.tryReadTChan control
      case maybeControlMessage of
        Nothing -> do
          newTime <- liftIO $ toNanoSecs <$> getTime Monotonic
          actions <- STM.atomically $ TQueue.flushTQueue actionQueue
          worldState <- STM.readTVarIO (world gameState)
          let
            camera = activeCamera worldState
          for_ actions $ \action ->
            case action of
              (MoveForward, b) -> STM.atomically $ STM.writeTVar (moveForward gameState) b
              (MoveBackward, b) -> STM.atomically $ STM.writeTVar (moveBackward gameState) b
              (StrafeLeft, b) -> STM.atomically $ STM.writeTVar (strafeLeft gameState) b
              (StrafeRight, b) -> STM.atomically $ STM.writeTVar (strafeRight gameState) b
              (Escape, _) -> STM.atomically $ STM.writeTVar (isRunning gameState) False
          let dt = newTime - prevTime

          (fwd, bwd, sl, sr, isRunning) <- STM.atomically $ do
            a <- STM.readTVar (moveForward gameState)
            b <- STM.readTVar (moveBackward gameState)
            c <- STM.readTVar (strafeLeft gameState)
            d <- STM.readTVar (strafeRight gameState)
            e <- STM.readTVar (isRunning gameState)
            pure (a,b,c,d,e)
           
          when (fwd) $ STM.atomically $ updateCamera (activeCamera worldState) (\v -> v + (V3 0.0 0.0 (camSpeed/frameDelay))) id
          when (bwd) $ STM.atomically $ updateCamera (activeCamera worldState) (\v -> v - (V3 0.0 0.0 (camSpeed/frameDelay))) id
          when (sl) $ STM.atomically $ updateCamera (activeCamera worldState) (\v -> v - (V3 (camSpeed/frameDelay) 0.0 0.0)) id
          when (sr) $ STM.atomically $ updateCamera (activeCamera worldState) (\v -> v + (V3 (camSpeed/frameDelay) 0.0 0.0)) id
          threadDelay (round frameDelay)
          when isRunning $ loop (tFPS) _gameState newTime
        Just Terminate -> do
          logI "terminating stateUpdate loop by signal"

  currentTime <- liftIO $ toNanoSecs <$> getTime Monotonic
  loop targetFPS gameState currentTime
  logI "stateUpdateLoop finished"
  putMVar finishedSemaphore ()


inputLoop :: MonadManaged m => Integer -> MVar () -> TQueue ActionEvent -> TChan ControlMessage -> m ()
inputLoop targetFPS finishedSemaphore actionQueue controlChannel = do
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
            actions = catMaybes $ map (payloadToActionEvent . SDL.eventPayload) events
          threadDelay (10000)
          STM.atomically $ for_ actions $ TQueue.writeTQueue actionQueue
          loop tFPS
        Just Terminate -> do
          logI "terminating input loop by signal"

  loop targetFPS
  logI "inputLoop finished"
  liftIO $ putMVar finishedSemaphore ()

updateCamera
  :: TVar Camera
  -> (Position -> Position)
  -> (Position -> Position)
  -> STM ()
updateCamera tvCamera positionModifier lookAtModifier = STM.modifyTVar' tvCamera $
  \camera@Camera{..} -> camera{camPos = positionModifier camPos, camLookAt = lookAtModifier camLookAt}

data Event

data KeyModifier
  = LShift
  | RShift
  | LCtrl
  | RCtrl
  | LAlt
  | RAlt
  | LGUI
  | RGUI
  | NumLock
  | CapsLock
  | AltGr
  deriving (Eq, Enum, Generic, Show)

type KeyBindings = HashMap ([KeyModifier],SDL.Keycode) (Action)
instance Hashable KeyModifier
instance Hashable SDL.Keycode

defaultBindings :: KeyBindings
defaultBindings = HashMap.fromList
  [ ( ([], SDL.KeycodeW), MoveForward)
  , ( ([], SDL.KeycodeS), MoveBackward)
  , ( ([], SDL.KeycodeA), StrafeLeft)
  , ( ([], SDL.KeycodeD), StrafeRight )
  -- quit
  , ( ([LShift], SDL.KeycodeQ), Escape )
  ]
 
updateGameState :: TVar GameState -> TQueue Event -> STM ()
updateGameState gameState tqEvents = do
  events <- TQueue.flushTQueue tqEvents
  pure ()

modifiersToList :: SDL.KeyModifier -> [KeyModifier]
modifiersToList SDL.KeyModifier{..} = []
  <> if keyModifierLeftShift then [LShift] else []
  <> if keyModifierRightShift then [RShift] else []
  <> if keyModifierLeftCtrl then [LCtrl] else []
  <> if keyModifierRightCtrl then [RCtrl] else []
  <> if keyModifierLeftAlt then [LAlt] else []
  <> if keyModifierRightAlt then [RAlt] else []
--  <> if keyModifierLeftGUI then [LGUI] else []
--  <> if keyModifierRightGUI then [RGUI] else []
--  <> if keyModifierNumLock then [NumLock] else []
--  <> if keyModifierCapsLock then [CapsLock] else []
--  <> if keyModifierAltGr then [AltGr] else []
 
payloadToActionEvent :: SDL.EventPayload -> Maybe ActionEvent
payloadToActionEvent SDL.QuitEvent = Just (Escape, True)
payloadToActionEvent (SDL.KeyboardEvent keyboardEvent) = keyToAction keyboardEvent
payloadToActionEvent _ = Nothing

keyToAction :: SDL.KeyboardEventData -> Maybe ActionEvent
keyToAction (SDL.KeyboardEventData _window motion isRepeated keysym)
  | isRepeated = Nothing
  | otherwise =
      let
        modifiers = modifiersToList (SDL.keysymModifier keysym)
        key = SDL.keysymKeycode keysym
      in case HashMap.lookup (modifiers, key) defaultBindings of
           Just action -> Just (action,motion == SDL.Pressed)
           Nothing -> Nothing

keyToAction _ = Nothing
