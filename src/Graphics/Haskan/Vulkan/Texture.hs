module Graphics.Haskan.Vulkan.Texture where

-- base
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits
import Data.Word (Word8)

-- juicypixels
import Codec.Picture

-- managed
import Control.Monad.Managed (MonadManaged)

-- vector
import qualified Data.Vector.Storable
import qualified Data.Vector.Storable as Vector

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan
import Graphics.Vulkan.Marshal.Create ((&*), set)

-- haskan
import qualified Graphics.Haskan.Vulkan.Buffer as Haskan
import qualified Graphics.Haskan.Vulkan.Memory as Haskan
import Graphics.Haskan.Resources (alloc, allocaAndPeek, allocaAndPeek_, throwVkResult)

readImageFromFile
  :: (MonadFail m, MonadIO m)
  => FilePath
  -> m ((Data.Vector.Storable.Vector Word8), Int, Int)
readImageFromFile filePath = do
  image <- liftIO $ readImageWithMetadata filePath >>=
    \case
      Right (dynamicImage, imageMetadata) -> pure dynamicImage
      Left e -> fail e

  let (ImageRGB8 (Image width height imageData)) = image
  pure (imageData, width, height)

managedTexture
  :: (MonadFail m, MonadManaged m)
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> FilePath -- Data.Vector.Storable.Vector Word8
  -> m Vulkan.VkBuffer
managedTexture pdev dev filePath = do
  (imgData, width, height) <- liftIO (readImageFromFile filePath)
  let
    dataList = Vector.toList imgData

  (stagingBuffer, stagingMemoryRequirement) <-
    Haskan.managedBuffer dev dataList Vulkan.VK_BUFFER_USAGE_TRANSFER_SRC_BIT

  stagingMemory <-
    Haskan.managedBufferMemory pdev dev stagingMemoryRequirement

  liftIO $ Haskan.copyDataToDeviceMemory dev stagingMemory dataList

  let
    imageExtent = Vulkan.createVk
      (  set @"width" (fromIntegral width)
      &* set @"height" (fromIntegral height)
      &* set @"depth" 1
      )
    createInfo = Vulkan.createVk
      (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO
      &* set @"pNext" Vulkan.VK_NULL
      &* set @"imageType" Vulkan.VK_IMAGE_TYPE_2D
      &* set @"extent" imageExtent
      &* set @"mipLevels" 1
      &* set @"arrayLayers" 1
      &* set @"format" Vulkan.VK_FORMAT_R8G8B8_SRGB
      &* set @"tiling" Vulkan.VK_IMAGE_TILING_OPTIMAL
      &* set @"initialLayout" Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
      &* set @"usage" (Vulkan.VK_IMAGE_USAGE_TRANSFER_DST_BIT .|. Vulkan.VK_IMAGE_USAGE_SAMPLED_BIT)
      &* set @"sharingMode" Vulkan.VK_SHARING_MODE_EXCLUSIVE
      &* set @"samples" Vulkan.VK_SAMPLE_COUNT_1_BIT
      &* set @"flags" Vulkan.VK_ZERO_FLAGS
      &* set @"queueFamilyIndexCount" 0
      &* set @"pQueueFamilyIndices" Vulkan.VK_NULL
      )

  image <- allocaAndPeek
    (Vulkan.vkCreateImage dev (Vulkan.unsafePtr createInfo) Vulkan.vkNullPtr)
--    (\ptr -> Vulkan.VkDestroyImage dev ptr Vulkan.vkNullPtr)

  imageMemoryRequirements <- allocaAndPeek_
    (Vulkan.vkGetImageMemoryRequirements dev image)

  imageMemory <-
    Haskan.allocateMemoryFor pdev dev imageMemoryRequirements [Vulkan.VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT]

  bindImageMemory dev image imageMemory 0
 
  pure stagingBuffer

bindImageMemory
  :: (MonadFail m, MonadIO m)
  => Vulkan.VkDevice
  -> Vulkan.VkImage
  -> Vulkan.VkDeviceMemory
  -> Vulkan.VkDeviceSize
  -> m ()
bindImageMemory dev image memory offset =
  liftIO (Vulkan.vkBindImageMemory dev image memory offset) >>= throwVkResult
