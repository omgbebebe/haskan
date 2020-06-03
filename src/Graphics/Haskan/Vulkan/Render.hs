module Graphics.Haskan.Vulkan.Render where

-- base
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Traversable (for)
import Data.Foldable (for_)
import qualified Foreign
import qualified Foreign.Marshal.Array

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Ext as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan
import Graphics.Vulkan.Marshal.Create (set, setListRef, (&*))

-- managed
import Control.Monad.Managed (MonadManaged)
-- haskan
import qualified Graphics.Haskan.Vulkan.CommandPool as CommandPool
import qualified Graphics.Haskan.Vulkan.CommandBuffer as CommandBuffer
import qualified Graphics.Haskan.Vulkan.DescriptorSet as DescriptorSet
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
import Graphics.Haskan.Resources (throwVkResult, allocaAndPeek, allocaAndPeekVkResult)

maxFramesInFlight :: Int
maxFramesInFlight = 2

data RenderContext =
  RenderContext { device :: Vulkan.VkDevice
                , swapchain :: Vulkan.VkSwapchainKHR
                , graphicsCommandBuffers :: [Vulkan.VkCommandBuffer]
                , graphicsQueueHandler :: Vulkan.VkQueue
                , presentQueueHandler :: Vulkan.VkQueue
--                , imageAvailableSemaphore :: Vulkan.VkSemaphore
                , renderFinishedFences :: [Vulkan.VkFence]
                , renderFinishedSemaphores :: [Vulkan.VkSemaphore]
                }
  deriving (Show)

type ImageIndex = Vulkan.Word32

data RenderResult
  = FrameOk ImageIndex
  | FrameSuboptimal ImageIndex
  | FrameOutOfDate
  | FrameFailed String
  deriving (Eq, Show)

createRenderContext
  :: (MonadIO m, MonadManaged m)
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> Vulkan.VkSurfaceKHR
  -> Vulkan.VkPipelineLayout
  -> Vulkan.VkShaderModule
  -> Vulkan.VkShaderModule
  -> [Vulkan.VkDescriptorSet]
  -> Vulkan.VkCommandPool
  -> Vulkan.VkQueue
  -> Vulkan.VkQueue
  -> [Vulkan.VkFence]
  -> [Vulkan.VkSemaphore]
  -> [Vulkan.VkBuffer]
  -> [Vulkan.VkBuffer]
  -> m RenderContext
createRenderContext pdev device surface pipelineLayout vertShader fragShader descriptorSets graphicsCommandPool
                    graphicsQueueHandler presentQueueHandler renderFinishedFences renderFinishedSemaphores vertexBuffers indexBuffers = do
  surfaceExtent <- PhysicalDevice.surfaceExtent pdev surface
  swapchain <- Swapchain.managedSwapchain device surface surfaceExtent
  -- TODO: embed imageViews somewhere
  images <- Swapchain.getSwapchainImages device swapchain
  imageViews <- for images (Swapchain.managedImageView device Swapchain.surfaceFormat)
  renderPass <- RenderPass.managedRenderPass device Swapchain.surfaceFormat
  graphicsPipeline <-
    GraphicsPipeline.managedGraphicsPipeline
      device
      Swapchain.surfaceFormat
      pipelineLayout
      renderPass
      vertShader
      fragShader
      surfaceExtent
  framebuffers <- for imageViews (Framebuffer.managedFramebuffer device renderPass surfaceExtent)

  graphicsCommandBuffers <- for framebuffers (\_ -> CommandBuffer.createCommandBuffer device graphicsCommandPool)
 
  for (zip3 framebuffers graphicsCommandBuffers descriptorSets)
    (\(fb, cb, ds) -> CommandBuffer.withCommandBuffer cb
      (RenderPass.withRenderPass cb renderPass fb surfaceExtent $ do
       GraphicsPipeline.cmdBindPipeline cb graphicsPipeline
       liftIO $ Foreign.Marshal.Array.withArray [ ds ] $ \dsPtr ->
         DescriptorSet.cmdBindDescriptorSets
           cb
           Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS
           pipelineLayout
           0
           1
           dsPtr
           0
           Vulkan.vkNullPtr
     
       liftIO $ Foreign.Marshal.Array.withArray vertexBuffers $ \bufferPtr ->
         Foreign.Marshal.Array.withArray [ 0 ] $ \offsetPtr ->
           Vulkan.vkCmdBindVertexBuffers cb 0 1 bufferPtr offsetPtr

       for_ indexBuffers $ \indexBuffer -> liftIO $
         Vulkan.vkCmdBindIndexBuffer cb indexBuffer 0 Vulkan.VK_INDEX_TYPE_UINT32

       CommandBuffer.cmdDraw cb
      )
    )

  pure RenderContext{..}


drawFrame :: (MonadFail m, MonadIO m) => RenderContext -> Vulkan.VkSemaphore -> Int -> m RenderResult
drawFrame ctx@RenderContext{..} imageAvailableSemaphore fenceIndex = do
  let

  (imageIndex, vkResult) <- liftIO $ allocaAndPeekVkResult $
    --Vulkan.vkAcquireNextImageKHR device swapchain 100 imageAvailableSemaphore Vulkan.VK_NULL_HANDLE
    Vulkan.vkAcquireNextImageKHR device swapchain maxBound imageAvailableSemaphore Vulkan.VK_NULL_HANDLE
 
  case vkResult of
    Vulkan.VK_SUCCESS -> FrameOk <$> renderImage ctx imageAvailableSemaphore fenceIndex imageIndex
    Vulkan.VK_SUBOPTIMAL_KHR -> pure $ FrameSuboptimal imageIndex
    Vulkan.VK_ERROR_OUT_OF_DATE_KHR -> pure FrameOutOfDate
    _ -> pure $ FrameFailed (show vkResult)

renderImage ctx@RenderContext{..} imageAvailableSemaphore fenceIndex imageIndex = do
  let
    commandBuffer = graphicsCommandBuffers !! (fromIntegral imageIndex)
    renderFinishedSemaphore = renderFinishedSemaphores !! (fromIntegral imageIndex)
    renderFinishedFence = renderFinishedFences !! fenceIndex
    submitInfo = Vulkan.createVk
      (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_SUBMIT_INFO
      &* set @"pNext" Vulkan.vkNullPtr
      &* set @"waitSemaphoreCount" 1
      &* setListRef @"pWaitSemaphores" [imageAvailableSemaphore]
      &* setListRef @"pWaitDstStageMask" [Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
      &* set @"commandBufferCount" 1
      &* setListRef @"pCommandBuffers" [commandBuffer]
      &* set @"signalSemaphoreCount" 1
      &* setListRef @"pSignalSemaphores" [renderFinishedSemaphore]
      )
  liftIO $ do
    Foreign.Marshal.Array.withArray [renderFinishedFence] $ \ptr -> do
      Vulkan.vkWaitForFences device 1 ptr Vulkan.VK_TRUE maxBound >>= throwVkResult
      Vulkan.vkResetFences device 1 ptr >>= throwVkResult
    Vulkan.vkQueueSubmit graphicsQueueHandler 1 (Vulkan.unsafePtr submitInfo) renderFinishedFence >>= throwVkResult
  pure (imageIndex)

presentFrame :: MonadIO m => RenderContext -> Vulkan.Word32 -> Vulkan.VkSemaphore -> m ()
presentFrame ctx@RenderContext{..} imageIndex renderFinishedSem = do
  let
    presentInfo = Vulkan.createVk
      (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
      &* set @"pNext" Vulkan.vkNullPtr
      &* set @"waitSemaphoreCount" 1
      &* setListRef @"pWaitSemaphores" [renderFinishedSem]
      &* set @"swapchainCount" 1
      &* setListRef @"pSwapchains" [swapchain]
      &* setListRef @"pImageIndices" [imageIndex]
      &* set @"pResults" Vulkan.vkNullPtr
      )
  liftIO $ do
    Vulkan.vkQueuePresentKHR presentQueueHandler (Vulkan.unsafePtr presentInfo)
  pure ()
