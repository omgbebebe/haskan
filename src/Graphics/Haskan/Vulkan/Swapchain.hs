module Graphics.Haskan.Vulkan.Swapchain where

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

managedSwapchain
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkSurfaceKHR
  -> Vulkan.VkExtent2D
  -> m Vulkan.VkSwapchainKHR
managedSwapchain dev surface extent = alloc "Swapchain"
  (createSwapchain dev surface extent)
  (\ptr -> Vulkan.vkDestroySwapchainKHR dev ptr Vulkan.vkNullPtr)

createSwapchain
  :: MonadIO m
  => Vulkan.VkDevice
  -> Vulkan.VkSurfaceKHR
  -> Vulkan.VkExtent2D
  -> m Vulkan.VkSwapchainKHR
createSwapchain dev surface extent = do
  let
    imageFormat = Vulkan.VK_FORMAT_B8G8R8A8_SRGB
    imageColorSpace = Vulkan.VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
    imageCount = 3 + 1
    presentMode = Vulkan.VK_PRESENT_MODE_MAILBOX_KHR
    preTransform = Vulkan.VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR
    createInfo = Vulkan.createVk
      (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
      &* set @"pNext" Vulkan.VK_NULL
      &* set @"surface" surface
      &* set @"minImageCount" (fromIntegral imageCount)
      &* set @"imageFormat" imageFormat
      &* set @"imageColorSpace" imageColorSpace
      &* set @"imageExtent" extent
      &* set @"imageArrayLayers" 1
      &* set @"imageUsage" Vulkan.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
      &* set @"imageSharingMode" Vulkan.VK_SHARING_MODE_EXCLUSIVE
      &* set @"queueFamilyIndexCount" 0
      &* set @"pQueueFamilyIndices" Vulkan.VK_NULL
      &* set @"compositeAlpha" Vulkan.VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
      &* set @"presentMode" presentMode
      &* set @"clipped" Vulkan.VK_TRUE
      &* set @"oldSwapchain" Vulkan.VK_NULL
      &* set @"preTransform" preTransform
      )

  swapchain <- liftIO $ allocaAndPeek (Vulkan.vkCreateSwapchainKHR dev (Vulkan.unsafePtr createInfo) Vulkan.vkNullPtr)
  pure swapchain

getSwapchainImages :: MonadIO m => Vulkan.VkDevice -> Vulkan.VkSwapchainKHR -> m [Vulkan.VkImage]
getSwapchainImages dev swapchain = liftIO $ peekVkList (Vulkan.vkGetSwapchainImagesKHR dev swapchain)

-- TODO: get SurfaceFormats from device

surfaceFormat :: Vulkan.VkSurfaceFormatKHR
surfaceFormat =
  let
    imageFormat = Vulkan.VK_FORMAT_B8G8R8A8_SRGB
    imageColorSpace = Vulkan.VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
  in Vulkan.createVk
       (  set @"format" imageFormat
       &* set @"colorSpace" imageColorSpace
       )

managedImageView
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkSurfaceFormatKHR
  -> Vulkan.VkImage
  -> m Vulkan.VkImageView
managedImageView dev surfaceFormat img = alloc "ImageView"
  (createImageView dev surfaceFormat img)
  (\ptr -> Vulkan.vkDestroyImageView dev ptr Vulkan.vkNullPtr)

createImageView
  :: MonadIO m
  => Vulkan.VkDevice
  -> Vulkan.VkSurfaceFormatKHR
  -> Vulkan.VkImage
  -> m Vulkan.VkImageView
createImageView dev surfaceFormat img = do
  let
    createInfo = Vulkan.createVk
      (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
      &* set @"pNext" Vulkan.VK_NULL
      &* set @"image" img
      &* set @"viewType" Vulkan.VK_IMAGE_VIEW_TYPE_2D
      &* set @"format" format
      &* set @"components" cmapping
      &* set @"subresourceRange" subresourceRange
      )
    cmapping = Vulkan.createVk
      (  set @"r" Vulkan.VK_COMPONENT_SWIZZLE_IDENTITY
      &* set @"g" Vulkan.VK_COMPONENT_SWIZZLE_IDENTITY
      &* set @"b" Vulkan.VK_COMPONENT_SWIZZLE_IDENTITY
      &* set @"a" Vulkan.VK_COMPONENT_SWIZZLE_IDENTITY
      )
    subresourceRange = Vulkan.createVk
      (  set @"aspectMask" Vulkan.VK_IMAGE_ASPECT_COLOR_BIT
      &* set @"baseMipLevel" 0
      &* set @"levelCount" 1
      &* set @"baseArrayLayer" 0
      &* set @"layerCount" 1
      )
    format = Vulkan.getField @"format" surfaceFormat
    in liftIO $ allocaAndPeek (Vulkan.vkCreateImageView dev (Vulkan.unsafePtr createInfo) Vulkan.VK_NULL)
