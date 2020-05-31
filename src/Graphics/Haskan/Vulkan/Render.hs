module Graphics.Haskan.Vulkan.Render where

-- base
import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.Marshal.Array

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Ext as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan
import Graphics.Vulkan.Marshal.Create (set, setListRef, (&*))

-- haskan
import Graphics.Haskan.Resources (throwVkResult, allocaAndPeek)

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

drawFrame :: MonadIO m => RenderContext -> Vulkan.VkSemaphore -> Int -> m Vulkan.Word32
drawFrame ctx@RenderContext{..} imageAvailableSemaphore fenceIndex = do
  let

  imageIndex <- liftIO $ allocaAndPeek $
    --Vulkan.vkAcquireNextImageKHR device swapchain maxBound imageAvailableSemaphore Vulkan.VK_NULL_HANDLE
    Vulkan.vkAcquireNextImageKHR device swapchain 100 imageAvailableSemaphore Vulkan.VK_NULL_HANDLE
--  liftIO $ putStr (show imageIndex <> " ")
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
