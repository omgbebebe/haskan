module Graphics.Haskan.Vulkan.DescriptorPool where

-- base
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Traversable (for)

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
