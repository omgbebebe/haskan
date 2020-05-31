module Graphics.Haskan.Vulkan.Framebuffer where

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
import Graphics.Haskan.Resources (alloc, alloc_, allocaAndPeek, allocaAndPeek_, peekVkList, peekVkList_)

managedFramebuffer
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkRenderPass
  -> Vulkan.VkExtent2D
  -> Vulkan.VkImageView
  -> m Vulkan.VkFramebuffer
managedFramebuffer dev renderPass extent imageView = alloc "Framebuffer"
  (createFramebuffer dev renderPass extent imageView)
  (\ptr -> Vulkan.vkDestroyFramebuffer dev ptr Vulkan.vkNullPtr)

createFramebuffer
  :: MonadIO m
  => Vulkan.VkDevice
  -> Vulkan.VkRenderPass
  -> Vulkan.VkExtent2D
  -> Vulkan.VkImageView
  -> m Vulkan.VkFramebuffer
createFramebuffer dev renderPass extent imageView = do
  let
    framebufferCI = Vulkan.createVk
      (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
      &* set @"pNext" Vulkan.VK_NULL
      &* set @"renderPass" renderPass
      &* set @"attachmentCount" 1
      &* setListRef @"pAttachments" [imageView]
      &* set @"width" (Vulkan.getField @"width" extent)
      &* set @"height" (Vulkan.getField @"height" extent)
      &* set @"layers" 1
      )
  liftIO $ allocaAndPeek (Vulkan.vkCreateFramebuffer dev (Vulkan.unsafePtr framebufferCI) Vulkan.VK_NULL)
