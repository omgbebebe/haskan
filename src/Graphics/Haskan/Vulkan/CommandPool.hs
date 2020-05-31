module Graphics.Haskan.Vulkan.CommandPool where

-- base
import Control.Monad (filterM, guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits ((.&.), (.|.))
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
import Graphics.Haskan.Resources (alloc, alloc_, allocaAndPeek, allocaAndPeek_, peekVkList, peekVkList_)

managedCommandPool
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Int
  -> m Vulkan.VkCommandPool
managedCommandPool dev qfi = alloc "Command pool"
  (createCommandPool dev qfi)
  (\ptr -> Vulkan.vkDestroyCommandPool dev ptr Vulkan.vkNullPtr)

createCommandPool
  :: MonadIO m
  => Vulkan.VkDevice
  -> Int
  -> m Vulkan.VkCommandPool
createCommandPool dev queueFamilyIndex = do
  let
    commandPoolCI = Vulkan.createVk
      (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
      &* set @"pNext" Vulkan.VK_NULL
      &* set @"queueFamilyIndex" (fromIntegral queueFamilyIndex)
      &* set @"flags" Vulkan.VK_ZERO_FLAGS
      )
  liftIO $ allocaAndPeek (Vulkan.vkCreateCommandPool dev (Vulkan.unsafePtr commandPoolCI) Vulkan.VK_NULL)
