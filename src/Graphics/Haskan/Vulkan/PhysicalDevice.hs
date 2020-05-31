module Graphics.Haskan.Vulkan.PhysicalDevice
  (selectPhysicalDevice
  ) where

-- base
import qualified Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits

-- pretty-simple
import Text.Pretty.Simple (pPrint)

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Ext as Vulkan

-- haskan
import Graphics.Haskan.Resources (alloc
                                 , alloc_
                                 , allocaAndPeek
                                 , allocaAndPeek_
                                 , peekVkList
                                 , peekVkList_
                                 )

selectPhysicalDevice :: MonadIO m => Vulkan.VkInstance -> Vulkan.VkSurfaceKHR -> m Vulkan.VkPhysicalDevice
selectPhysicalDevice inst surface = do
  physicalDevices <- liftIO $ peekVkList (Vulkan.vkEnumeratePhysicalDevices inst)
  peekPhysicalDevice physicalDevices

peekPhysicalDevice :: MonadIO m => [Vulkan.VkPhysicalDevice] -> m Vulkan.VkPhysicalDevice
peekPhysicalDevice = pure . head
