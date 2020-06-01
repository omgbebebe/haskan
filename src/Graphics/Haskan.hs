module Graphics.Haskan where

-- base
--import Control.Concurrent (threadDelay)
--import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (replicateM)
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)

-- linear
import Linear (V2(..), V3(..))

-- managed
import Control.Monad.Managed (runManaged)

-- sdl2
import qualified SDL

-- haskan
--import qualified Graphics.Haskan.Engine as Engine
import qualified Graphics.Haskan.Events as Events
import qualified Graphics.Haskan.Vertex (Vertex(..))

import Graphics.Haskan.Vulkan.Render (RenderContext(..), drawFrame, presentFrame)
import qualified Graphics.Haskan.Vulkan.Buffer as Buffer
import qualified Graphics.Haskan.Vulkan.Render as Render
--import qualified Graphics.Haskan.Vulkan.CommandPool as CommandPool
import qualified Graphics.Haskan.Vulkan.CommandPool as CommandPool
--import qualified Graphics.Haskan.Vulkan.CommandBuffer as CommandBuffer
import qualified Graphics.Haskan.Vulkan.Device as Device
import qualified Graphics.Haskan.Vulkan.Fence as Fence
--import qualified Graphics.Haskan.Vulkan.Framebuffer as Framebuffer
--import qualified Graphics.Haskan.Vulkan.GraphicsPipeline as GraphicsPipeline
import qualified Graphics.Haskan.Vulkan.Instance as Instance
import qualified Graphics.Haskan.Vulkan.PipelineLayout as PipelineLayout
import qualified Graphics.Haskan.Vulkan.PhysicalDevice as PhysicalDevice
--import qualified Graphics.Haskan.Vulkan.RenderPass as RenderPass
import qualified Graphics.Haskan.Vulkan.Semaphore as Semaphore
import qualified Graphics.Haskan.Vulkan.ShaderModule as ShaderModule
--import qualified Graphics.Haskan.Vulkan.Swapchain as Swapchain
import qualified Graphics.Haskan.Window as Window

import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
 
import Control.Monad.Managed (MonadManaged, with)
 
data QueueFamily
  = Graphics
  | Compute
  | Transfer
  | Sparse
 
--init :: MonadIO m => Text -> m ()
initHaskan :: Text -> IO ()
initHaskan title = runManaged $ do
  Events.managedEvents
  let initWidth = 1920
      initHeight = 1080
  (windowExts, window) <- Window.managedWindow title (initWidth, initHeight)
  (inst, layers)   <- Instance.managedInstance windowExts
  surface          <- Window.managedSurface inst window
  physicalDevice   <- PhysicalDevice.selectPhysicalDevice inst surface

  (device, (graphicsQueueFamilyIndex, presentQueueFamilyIndex)) <- Device.managedRenderDevice physicalDevice surface layers
  Window.showWindow window
  appLoop surface physicalDevice device graphicsQueueFamilyIndex presentQueueFamilyIndex

renderLoop :: (MonadFail m, MonadIO m) => RenderContext -> Int -> [Vulkan.VkSemaphore] -> m Bool
renderLoop ctx@RenderContext{..} frameNumber imageAvailableSemaphores = do
  events <- SDL.pollEvents
  let
    eventIsQPress event =
      case SDL.eventPayload event of
        SDL.KeyboardEvent keyboardEvent ->
          SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
          SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
        _ -> False
    qPressed = any eventIsQPress events

  needRestart <- do
    let imageAvailableSemaphore = imageAvailableSemaphores !! (frameNumber)
    res <- liftIO $ drawFrame ctx imageAvailableSemaphore frameNumber
    case res of
      Render.FrameOk imageIndex -> do
        liftIO $ presentFrame ctx imageIndex (renderFinishedSemaphores !! (fromIntegral imageIndex))
        pure False
      Render.FrameSuboptimal _ -> fail "suboptimal"
      Render.FrameOutOfDate -> do
        liftIO $ putStrLn "resizing swapchain"
        pure True
      Render.FrameFailed err -> fail err
 
  if (qPressed || needRestart)
  then do
    _ <- liftIO $ Vulkan.vkDeviceWaitIdle device
    liftIO $ putStrLn "===================== end renderLoop"
    pure qPressed
  else do
    --SDL.delay 200
    renderLoop ctx ((frameNumber + 1) `mod` Render.maxFramesInFlight) imageAvailableSemaphores

appLoop
  :: MonadManaged m
  => Vulkan.VkPtr Vulkan.VkSurfaceKHR_T
  -> Vulkan.Ptr Vulkan.VkPhysicalDevice_T
  -> Vulkan.VkDevice
  -> Int
  -> Int
  -> m ()
appLoop surface physicalDevice device graphicsQueueFamilyIndex presentQueueFamilyIndex = do
  liftIO $ putStrLn "starting render loop"
  graphicsQueueHandler <- Device.getDeviceQueueHandler device graphicsQueueFamilyIndex 0
  presentQueueHandler <- Device.getDeviceQueueHandler device presentQueueFamilyIndex 0

  vertShader <- ShaderModule.managedShaderModule device "data/shaders/buffers/vert.spv"
  fragShader <- ShaderModule.managedShaderModule device "data/shaders/buffers/frag.spv"

  pipelineLayout <- PipelineLayout.managedPipelineLayout device
  graphicsCommandPool <- CommandPool.managedCommandPool device graphicsQueueFamilyIndex
--  presentCommandPool <- CommandPool.managedCommandPool device presentQueueFamilyIndex
--  presentCommandBuffers   <- for framebuffers (\_ -> CommandBuffer.createCommandBuffer device presentCommandPool)

  imageAvailableSemaphores <- replicateM Render.maxFramesInFlight (Semaphore.managedSemaphore device)
  renderFinishedSemaphores <- replicateM 4 (Semaphore.managedSemaphore device)
  renderFinishedFences <- replicateM Render.maxFramesInFlight (Fence.managedFence device)

  vertexBuffer <-
    Buffer.managedVertexBuffer
    physicalDevice
    device
    [V2 (V3   0.5  (-0.5) 0.0) (V3 1.0 0.0 0.0)
    ,V2 (V3   0.5  ( 0.5) 0.0) (V3 0.0 1.0 0.0)
    ,V2 (V3 (-0.5) ( 0.5) 0.0) (V3 1.0 1.0 0.0)
    ]
  --indexBuffer <- Buffer.managedIndexBuffer physicalDevice device

  let
    mkRenderContext = Render.createRenderContext
      physicalDevice
      device
      surface
      pipelineLayout
      vertShader
      fragShader
      graphicsCommandPool
      graphicsQueueHandler
      presentQueueHandler
      renderFinishedFences
      renderFinishedSemaphores
      [vertexBuffer]

  -- need to fetch RenderContext from Managed monad to allow proper resource deallocation
  let
    loop exit = do
      if exit
      then pure ()
      else do
        qPressed <- liftIO $ with mkRenderContext $ \context -> renderLoop context 0 imageAvailableSemaphores
        loop qPressed

  loop False
  pure ()
