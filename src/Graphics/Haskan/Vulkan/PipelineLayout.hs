module Graphics.Haskan.Vulkan.PipelineLayout where

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

managedPipelineLayout :: MonadManaged m => Vulkan.VkDevice -> m Vulkan.VkPipelineLayout
managedPipelineLayout dev = alloc "PipelineLayout"
  (createPipelineLayout dev)
  (\ptr -> Vulkan.vkDestroyPipelineLayout dev ptr Vulkan.vkNullPtr)

createPipelineLayout :: MonadIO m => Vulkan.VkDevice -> m Vulkan.VkPipelineLayout
createPipelineLayout dev =
  let
    createInfo = Vulkan.createVk
      (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
      &* set @"pNext" Vulkan.VK_NULL
      &* set @"flags" Vulkan.VK_ZERO_FLAGS
      &* set @"setLayoutCount" 0
      &* set @"pSetLayouts" Vulkan.VK_NULL
      &* set @"pushConstantRangeCount" 0
      &* set @"pPushConstantRanges" Vulkan.VK_NULL
      )
  in liftIO $ allocaAndPeek $ Vulkan.vkCreatePipelineLayout dev (Vulkan.unsafePtr createInfo) Vulkan.VK_NULL
