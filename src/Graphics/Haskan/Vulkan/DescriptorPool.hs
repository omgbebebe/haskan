module Graphics.Haskan.Vulkan.DescriptorPool where

-- base
import Control.Monad.IO.Class (MonadIO, liftIO)

-- managed
import Control.Monad.Managed (MonadManaged)

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan
import Graphics.Vulkan.Marshal.Create (set, setListRef, (&*))

-- haskan
import Graphics.Haskan.Resources (alloc, allocaAndPeek)

managedDescriptorPool :: MonadManaged m => Vulkan.VkDevice -> Int -> m Vulkan.VkDescriptorPool
managedDescriptorPool dev imageViewCount = alloc "DescriptorPool"
  (createDescriptorPool dev imageViewCount)
  (\ptr -> Vulkan.vkDestroyDescriptorPool dev ptr Vulkan.vkNullPtr)

createDescriptorPool :: MonadIO m => Vulkan.VkDevice -> Int -> m Vulkan.VkDescriptorPool
createDescriptorPool dev imageViewCount = do
  let
    poolSize  = Vulkan.createVk
      (  set @"type" Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
      &* set @"descriptorCount" (fromIntegral imageViewCount)
      )
    createInfo = Vulkan.createVk
      (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
      &* set @"pNext" Vulkan.VK_NULL
      &* set @"flags" Vulkan.VK_ZERO_FLAGS
      &* set @"poolSizeCount" 1
      &* setListRef @"pPoolSizes" [poolSize]
      &* set @"maxSets" (fromIntegral imageViewCount)
      )
    in liftIO $ allocaAndPeek (Vulkan.vkCreateDescriptorPool dev (Vulkan.unsafePtr createInfo) Vulkan.vkNullPtr)
