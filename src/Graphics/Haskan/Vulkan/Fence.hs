module Graphics.Haskan.Vulkan.Fence where

-- base
import Control.Monad.IO.Class (MonadIO, liftIO)

-- managed
import Control.Monad.Managed (MonadManaged)

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Ext as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan
import Graphics.Vulkan.Marshal.Create (set, setListRef, (&*))

-- haskan
import Graphics.Haskan.Resources (alloc, alloc_, allocaAndPeek, peekVkList)

managedFence :: MonadManaged m => Vulkan.VkDevice -> m Vulkan.VkFence
managedFence dev = alloc "Vulkan Fence"
  (createFence dev)
  (\ptr -> Vulkan.vkDestroyFence dev ptr Vulkan.vkNullPtr)

createFence :: MonadIO m => Vulkan.VkDevice -> m Vulkan.VkFence
createFence dev =
  let
    createInfo = Vulkan.createVk
        (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_FENCE_CREATE_INFO
        &* set @"pNext" Vulkan.vkNullPtr
        &* set @"flags" Vulkan.VK_FENCE_CREATE_SIGNALED_BIT
        )
  in liftIO $ allocaAndPeek (Vulkan.vkCreateFence dev (Vulkan.unsafePtr createInfo) Vulkan.VK_NULL)
