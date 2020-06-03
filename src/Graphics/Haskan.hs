module Graphics.Haskan where

-- base
--import Control.Concurrent (threadDelay)
--import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (replicateM)
import Data.Foldable (for_)
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)
import qualified Foreign.C

-- clock
--import qualified System.Clock

-- linear
import Linear (V2(..), V3(..))
import Linear.Matrix (M44(..), (!*!))
import qualified Linear.Matrix
import qualified Linear.Projection

-- managed
import Control.Monad.Managed (runManaged)

-- sdl2
import qualified SDL

-- vector
import qualified Data.Vector as Vector

-- haskan
--import qualified Graphics.Haskan.Engine as Engine
import qualified Graphics.Haskan.Events as Events
import Graphics.Haskan.Vertex (Vertex)

import Graphics.Haskan.Vulkan.Render (RenderContext(..), drawFrame, presentFrame)
import qualified Graphics.Haskan.Vulkan.Buffer as Buffer
import qualified Graphics.Haskan.Vulkan.Render as Render
--import qualified Graphics.Haskan.Vulkan.CommandPool as CommandPool
import qualified Graphics.Haskan.Vulkan.CommandPool as CommandPool
import qualified Graphics.Haskan.Vulkan.DescriptorPool as DescriptorPool
import qualified Graphics.Haskan.Vulkan.DescriptorSet as DescriptorSet
import qualified Graphics.Haskan.Vulkan.DescriptorSetLayout as DescriptorSetLayout
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
  :: (MonadFail m, MonadManaged m)
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

  vertShader <- ShaderModule.managedShaderModule device "data/shaders/mvp/vert.spv"
  fragShader <- ShaderModule.managedShaderModule device "data/shaders/mvp/frag.spv"

  descriptorSetLayout <- DescriptorSetLayout.managedDescriptorSetLayout device

  descriptorPool <- DescriptorPool.managedDescriptorPool device 4 -- imageViewCount here
  descriptorSets <- replicateM 4 (DescriptorSet.allocateDescriptorSet device descriptorPool [descriptorSetLayout])

  pipelineLayout <- PipelineLayout.managedPipelineLayout device [descriptorSetLayout]
  graphicsCommandPool <- CommandPool.managedCommandPool device graphicsQueueFamilyIndex
--  presentCommandPool <- CommandPool.managedCommandPool device presentQueueFamilyIndex
--  presentCommandBuffers   <- for framebuffers (\_ -> CommandBuffer.createCommandBuffer device presentCommandPool)

  imageAvailableSemaphores <- replicateM Render.maxFramesInFlight (Semaphore.managedSemaphore device)
  renderFinishedSemaphores <- replicateM 4 (Semaphore.managedSemaphore device)
  renderFinishedFences <- replicateM Render.maxFramesInFlight (Fence.managedFence device)

  let
    zPos = (5.0)
    vertices =
      [V2 (V3 (-1.0) ( 1.0) zPos) (V3 1.0 0.0 0.0)
      ,V2 (V3 (-1.0) (-1.0) zPos) (V3 0.0 1.0 0.0)
      ,V2 (V3 ( 1.0) (-1.0) zPos) (V3 1.0 1.0 0.0)
      ,V2 (V3 ( 1.0) ( 1.0) zPos) (V3 1.0 1.0 1.0)
      ]
    indices = [ 0, 1, 2
              , 2, 3, 0
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


  mvpBuffer <-
    let
      view =
        Linear.Projection.lookAt (V3 0.0 0.0 (-5.0)) (V3 0.0 0.0 0.0) (V3 0.0 (-1.0) 0.0)
      model =
        Linear.Matrix.identity
      projection =
        Linear.Projection.perspective
        (pi / 6) -- FOV
        (16/9) -- aspect ratio
        0.1 -- near plane
        100.0 -- far plane

      modelViewProjection :: M44 Foreign.C.CFloat
      modelViewProjection =
        Linear.Matrix.transpose (projection !*! view !*! model)
    in Buffer.managedUniformBuffer
       physicalDevice
       device
       [ modelViewProjection ]


  for_ descriptorSets $ \descriptorSet -> DescriptorSet.updateDescriptorSets device descriptorSet mvpBuffer
  --indexBuffer <- Buffer.managedIndexBuffer physicalDevice device

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
  let
    loop exit = do
      if exit
      then pure ()
      else do
        qPressed <- liftIO $ with mkRenderContext $ \context -> renderLoop context 0 imageAvailableSemaphores
        loop qPressed

  loop False
  pure ()
