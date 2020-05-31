module Graphics.Haskan.Vulkan.CommandBuffer where

-- base
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Traversable (for)
import qualified Foreign.Ptr
-- managed
import Control.Monad.Managed (MonadManaged)

-- pretty-simple
import Text.Pretty.Simple

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Ext as Vulkan
import qualified Graphics.Vulkan.Ext.VK_KHR_surface as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan
import Graphics.Vulkan.Marshal.Create (set, setListRef, setStrListRef, (&*))

-- haskan
import Graphics.Haskan.Resources (alloc, alloc_, allocaAndPeek, allocaAndPeek_, peekVkList, peekVkList_, throwVkResult)

createCommandBuffer
  :: MonadIO m
  => Vulkan.VkDevice
  -> Vulkan.VkCommandPool
  -> m Vulkan.VkCommandBuffer
createCommandBuffer dev commandPool =
  let
    createInfo = Vulkan.createVk
      (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
      &* set @"pNext" Vulkan.VK_NULL
      &* set @"commandPool" commandPool
      &* set @"level" Vulkan.VK_COMMAND_BUFFER_LEVEL_PRIMARY
      &* set @"commandBufferCount" 1
      )
  in liftIO $ allocaAndPeek (Vulkan.vkAllocateCommandBuffers dev (Vulkan.unsafePtr createInfo))

withCommandBuffer :: MonadIO m => Vulkan.VkCommandBuffer -> m a -> m a
withCommandBuffer commandBuffer action =
  let
    commandBufferBeginInfo = Vulkan.createVk
      (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
      &* set @"pNext" Vulkan.vkNullPtr
      &* set @"flags" Vulkan.VK_ZERO_FLAGS
      &* set @"pInheritanceInfo" Vulkan.vkNullPtr
      )
    begin = liftIO $ Vulkan.vkBeginCommandBuffer commandBuffer (Vulkan.unsafePtr commandBufferBeginInfo) >>= throwVkResult
    end = liftIO $ Vulkan.vkEndCommandBuffer commandBuffer >>= throwVkResult

  in (begin *> action <* end)

cmdDraw :: MonadIO m => Vulkan.VkCommandBuffer -> m ()
cmdDraw commandBuffer = liftIO $ Vulkan.vkCmdDraw commandBuffer 3 1 0 0
