module Graphics.Haskan.Vulkan.DescriptorSetLayout where

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

managedDescriptorSetLayout :: MonadManaged m => Vulkan.VkDevice -> m Vulkan.VkDescriptorSetLayout
managedDescriptorSetLayout dev = alloc "DescriptorSetLayout"
  (createDescriptorSetLayout dev)
  (\ptr -> Vulkan.vkDestroyDescriptorSetLayout dev ptr Vulkan.vkNullPtr)

createDescriptorSetLayout :: MonadIO m => Vulkan.VkDevice -> m Vulkan.VkDescriptorSetLayout
createDescriptorSetLayout dev = do
  let
    binding = Vulkan.createVk
      (  set @"binding" 0
      &* set @"descriptorType" Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
      &* set @"descriptorCount" 1
      &* set @"stageFlags" Vulkan.VK_SHADER_STAGE_VERTEX_BIT
      &* set @"pImmutableSamplers" Vulkan.VK_NULL
      )
    createInfo = Vulkan.createVk
      (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
      &* set @"pNext" Vulkan.VK_NULL
      &* set @"flags" Vulkan.VK_ZERO_FLAGS
      &* set @"bindingCount" 1
      &* setListRef @"pBindings" [ binding ]
      )
    in allocaAndPeek (Vulkan.vkCreateDescriptorSetLayout dev (Vulkan.unsafePtr createInfo) Vulkan.vkNullPtr)
