{-# language  DuplicateRecordFields #-}
module Graphics.Haskan.Vulkan.Types where

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan

data StaticRenderContext
  = StaticRenderContext { surface :: Vulkan.VkSurfaceKHR
                        , physicalDedice :: Vulkan.VkPhysicalDevice
                        , device :: Vulkan.VkDevice
                        , graphicsQueueFamilyIndex :: QueueFamilyIndex
                        , presentQueueFamilyIndex :: QueueFamilyIndex
                        }

data RenderContext
  = RenderContext { device :: Vulkan.VkDevice
                  , swapchain :: Vulkan.VkSwapchainKHR
                  , graphicsCommandBuffers :: [Vulkan.VkCommandBuffer]
                  , graphicsQueueHandler :: Vulkan.VkQueue
                  , presentQueueHandler :: Vulkan.VkQueue
                  , renderFinishedFences :: [Vulkan.VkFence]
                  , renderFinishedSemaphores :: [Vulkan.VkSemaphore]
                  }

type QueueFamilyIndex = Vulkan.Word32
type ImageIndex = Vulkan.Word32

data RenderResult
  = FrameOk ImageIndex
  | FrameSuboptimal ImageIndex
  | FrameOutOfDate
  | FrameFailed String
  deriving (Eq, Show)
