module Graphics.Haskan.Vulkan.Memory (managedMemoryFor, allocateMemoryFor) where

-- base
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits
import Data.Foldable (for_)
import qualified Foreign
import qualified Foreign.Marshal

-- managed
import Control.Monad.Managed (MonadManaged)

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Ext as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan
import Graphics.Vulkan.Marshal.Create (set, setListRef, setStrListRef, (&*))

-- haskan
import Graphics.Haskan.Resources (alloc, alloc_, allocaAndPeek, allocaAndPeek_, peekVkList, peekVkList_, throwVkResult)

managedMemoryFor
  :: MonadManaged m
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> Vulkan.VkMemoryRequirements
  -> [ Vulkan.VkMemoryPropertyFlags ]
  -> m Vulkan.VkDeviceMemory
managedMemoryFor pdev dev memoryRequirements memoryRequiredFlags = alloc "Memory region"
  (allocateMemoryFor pdev dev memoryRequirements memoryRequiredFlags)
  (\ptr -> Vulkan.vkFreeMemory dev ptr Vulkan.vkNullPtr)

allocateMemoryFor
  :: (MonadFail m, MonadIO m)
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> Vulkan.VkMemoryRequirements
  -> [ Vulkan.VkMemoryPropertyFlags ]
  -> m Vulkan.VkDeviceMemory
allocateMemoryFor pdev dev memoryRequirements memoryRequiredFlags = do
  memoryProperties <-
    allocaAndPeek_ (Vulkan.vkGetPhysicalDeviceMemoryProperties pdev)
  let
    memoryTypeCount =
      Vulkan.getField @"memoryTypeCount" memoryProperties

  memoryTypes <-
    liftIO $
    Foreign.Marshal.peekArray
      @Vulkan.VkMemoryType
      ( fromIntegral memoryTypeCount )
      ( Vulkan.unsafePtr memoryProperties
        `Foreign.plusPtr` Vulkan.fieldOffset @"memoryTypes" @Vulkan.VkPhysicalDeviceMemoryProperties
      )

  let
    possibleMemoryTypeIndices = do
      (i, memoryType) <-
        zip [0..] memoryTypes

      guard
        (testBit
         (Vulkan.getField @"memoryTypeBits" memoryRequirements)
         (fromIntegral i)
        )

      for_
        memoryRequiredFlags
        (\f ->
           guard (Vulkan.getField @"propertyFlags" memoryType .&. f > zeroBits)
        )

      pure i

  memoryTypeIndex <-
    case possibleMemoryTypeIndices of
      [] -> fail "required memory type not found"
      (i:_) -> pure i

  let
    allocateInfo = Vulkan.createVk
      (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
      &* set @"pNext" Vulkan.VK_NULL
      &* set @"allocationSize" (Vulkan.getField @"size" memoryRequirements)
      &* set @"memoryTypeIndex" memoryTypeIndex
      )

  allocaAndPeek (Vulkan.vkAllocateMemory dev (Vulkan.unsafePtr allocateInfo) Vulkan.vkNullPtr)
