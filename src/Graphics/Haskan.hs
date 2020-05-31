module Graphics.Haskan where

-- base
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless, replicateM)
import Data.Text (Text)
import Data.Traversable (for)
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

  (device
    , (graphicsQueueFamilyIndex, presentQueueFamilyIndex)
    ) <- Device.managedRenderDevice physicalDevice surface layers
  graphicsQueueHandler <- Device.getDeviceQueueHandler device graphicsQueueFamilyIndex 0
  presentQueueHandler <- Device.getDeviceQueueHandler device presentQueueFamilyIndex 0
  swapchain <- Swapchain.managedSwapchain device surface
  -- TODO: embed imageViews somewhere
  images <- Swapchain.getSwapchainImages device swapchain
  imageViews <- for images (Swapchain.managedImageView device Swapchain.surfaceFormat)

  vertShader <- ShaderModule.managedShaderModule device "data/shaders/static/vert.spv"
  fragShader <- ShaderModule.managedShaderModule device "data/shaders/static/frag.spv"

  renderPass <- RenderPass.managedRenderPass device Swapchain.surfaceFormat

  pipelineLayout <- PipelineLayout.managedPipelineLayout device
  graphicsPipeline <-
    GraphicsPipeline.managedGraphicsPipeline
      device
      Swapchain.surfaceFormat
      pipelineLayout
      renderPass
      vertShader
      fragShader
      Swapchain.swapchainExtent

  framebuffers <- for imageViews (Framebuffer.managedFramebuffer device renderPass Swapchain.swapchainExtent)
  graphicsCommandPool <- CommandPool.managedCommandPool device graphicsQueueFamilyIndex
--  presentCommandPool <- CommandPool.managedCommandPool device presentQueueFamilyIndex
  graphicsCommandBuffers   <- for framebuffers (\_ -> CommandBuffer.createCommandBuffer device graphicsCommandPool)
--  presentCommandBuffers   <- for framebuffers (\_ -> CommandBuffer.createCommandBuffer device presentCommandPool)

  imageAvailableSemaphores <- replicateM Render.maxFramesInFlight (Semaphore.managedSemaphore device)
  renderFinishedSemaphores <- for imageViews (\_ -> Semaphore.managedSemaphore device)
  renderFinishedFences <- replicateM Render.maxFramesInFlight (Fence.managedFence device)

  for (zip framebuffers graphicsCommandBuffers)
    (\(fb, cb) -> CommandBuffer.withCommandBuffer cb
      (RenderPass.withRenderPass cb renderPass fb Swapchain.swapchainExtent $ do
       GraphicsPipeline.cmdBindPipeline cb graphicsPipeline
       CommandBuffer.cmdDraw cb
      )
    )
   
  Window.showWindow window

  let
    ctx = RenderContext{..}
    appLoop ctx frameNumber = do
      events <- SDL.pollEvents
      let
        eventIsQPress event =
          case SDL.eventPayload event of
            SDL.KeyboardEvent keyboardEvent ->
              SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
              SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
            _ -> False
        qPressed = any eventIsQPress events

      liftIO $ do
        let imageAvailableSemaphore = imageAvailableSemaphores !! (frameNumber)
        imageIndex <- drawFrame ctx imageAvailableSemaphore frameNumber
        presentFrame ctx imageIndex (renderFinishedSemaphores !! (fromIntegral imageIndex))
      --liftIO $ drawFrame device swapchain imageAvailableSemaphore renderFinishedSemaphore graphicsCommandBuffers graphicsQueueHandler
      --liftIO $ presentFrame device swapchain imageAvailableSemaphore renderFinishedSemaphore graphicsCommandBuffers graphicsQueueHandler
--      SDL.delay 20
      unless qPressed (appLoop ctx ((frameNumber + 1) `mod` Render.maxFramesInFlight))

  appLoop ctx 0
  liftIO $ Vulkan.vkDeviceWaitIdle device
  {-
  Engine.mainLoop RenderContext{..}
  -}
  pure ()
