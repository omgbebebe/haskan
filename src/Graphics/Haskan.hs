{-# language RankNTypes #-}
module Graphics.Haskan where

-- base
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless, replicateM, forever)
import Data.Text (Text)
import Data.Traversable (for)
import System.Mem
-- managed
import Control.Monad.Managed (runManaged)

-- sdl2
import qualified SDL

-- haskan
import qualified Graphics.Haskan.Engine as Engine
import qualified Graphics.Haskan.Events as Events
import Graphics.Haskan.Vulkan.Render (RenderContext(..), drawFrame, presentFrame)
import qualified Graphics.Haskan.Vulkan.Render as Render
--import qualified Graphics.Haskan.Vulkan.CommandPool as CommandPool
import qualified Graphics.Haskan.Vulkan.CommandPool as CommandPool
import qualified Graphics.Haskan.Vulkan.CommandBuffer as CommandBuffer
import qualified Graphics.Haskan.Vulkan.Device as Device
import qualified Graphics.Haskan.Vulkan.Fence as Fence
import qualified Graphics.Haskan.Vulkan.Framebuffer as Framebuffer
import qualified Graphics.Haskan.Vulkan.GraphicsPipeline as GraphicsPipeline
import qualified Graphics.Haskan.Vulkan.Instance as Instance
import qualified Graphics.Haskan.Vulkan.PipelineLayout as PipelineLayout
import qualified Graphics.Haskan.Vulkan.PhysicalDevice as PhysicalDevice
import qualified Graphics.Haskan.Vulkan.RenderPass as RenderPass
import qualified Graphics.Haskan.Vulkan.Semaphore as Semaphore
import qualified Graphics.Haskan.Vulkan.ShaderModule as ShaderModule
import qualified Graphics.Haskan.Vulkan.Swapchain as Swapchain
import qualified Graphics.Haskan.Window as Window

import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
 
import Control.Monad.Managed (MonadManaged, using, managed, managed_, with)
 
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
  forever $ do
    appLoop window surface physicalDevice device graphicsQueueFamilyIndex presentQueueFamilyIndex
    liftIO $ do
      putStrLn "restarting appLoop..."
      threadDelay (10^6)

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
      --liftIO $ drawFrame device swapchain imageAvailableSemaphore renderFinishedSemaphore graphicsCommandBuffers graphicsQueueHandler
      --liftIO $ presentFrame device swapchain imageAvailableSemaphore renderFinishedSemaphore graphicsCommandBuffers graphicsQueueHandler
  rr <- liftIO $ bracket
    (putStrLn "(bracket+++) allocating" *> Semaphore.createSemaphore device)
    (\ptr -> putStrLn ("(bracket) do something with res " <> show ptr))
    (\ptr -> do
        putStrLn "(bracket---) deallocating"
        Vulkan.vkDestroySemaphore device ptr Vulkan.vkNullPtr
    )
  let
    withSem :: forall r . (Vulkan.VkSemaphore -> IO r) -> IO r
    withSem f =  bracket
      (putStrLn "(managed+++) allocating" *> Semaphore.createSemaphore device)
      (\ptr -> do
          putStrLn "(managed--) deallocating"
          Vulkan.vkDestroySemaphore device ptr Vulkan.vkNullPtr
      )
      f
--  liftIO $ withSem (\ptr -> putStrLn ("(test) " <> show ptr))
  let r2 = managed $ withSem
  liftIO $ with r2 (\x -> putStrLn ("(bracket) " <> show x))
{-
  r2 <- managed $ bracket
    (putStrLn "(managed+++) allocating new managed res" *> Semaphore.createSemaphore device)
    (\ptr -> do
        putStrLn "(managed---) deallocating sem via managed"
        Vulkan.vkDestroySemaphore device ptr Vulkan.vkNullPtr
    )
  liftIO $ putStrLn ("(managed) do something with res" <> show r2)
-}
  SDL.delay 200
  unless (qPressed || needRestart) (renderLoop ctx ((frameNumber + 1) `mod` Render.maxFramesInFlight) imageAvailableSemaphores)
  liftIO $ Vulkan.vkDeviceWaitIdle device
  pure ()

appLoop window surface physicalDevice device graphicsQueueFamilyIndex presentQueueFamilyIndex = do
  liftIO $ putStrLn "starting render loop"
  graphicsQueueHandler <- Device.getDeviceQueueHandler device graphicsQueueFamilyIndex 0
  presentQueueHandler <- Device.getDeviceQueueHandler device presentQueueFamilyIndex 0

  vertShader <- ShaderModule.managedShaderModule device "data/shaders/static/vert.spv"
  fragShader <- ShaderModule.managedShaderModule device "data/shaders/static/frag.spv"


  pipelineLayout <- PipelineLayout.managedPipelineLayout device
  graphicsCommandPool <- CommandPool.managedCommandPool device graphicsQueueFamilyIndex
--  presentCommandPool <- CommandPool.managedCommandPool device presentQueueFamilyIndex
--  presentCommandBuffers   <- for framebuffers (\_ -> CommandBuffer.createCommandBuffer device presentCommandPool)

  imageAvailableSemaphores <- replicateM Render.maxFramesInFlight (Semaphore.managedSemaphore device)
  renderFinishedSemaphores <- replicateM 4 (Semaphore.managedSemaphore device)
  renderFinishedFences <- replicateM Render.maxFramesInFlight (Fence.managedFence device)


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

  ctx <- mkRenderContext
  renderLoop ctx 0 imageAvailableSemaphores
  liftIO $ putStrLn "exiting appLoop"

  liftIO $ Vulkan.vkDeviceWaitIdle device
  pure ()
