module Graphics.Haskan.Vulkan.RenderPass where

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
import Graphics.Vulkan.Marshal.Create (set, setAt, setListRef, setStrListRef, (&*))

-- haskan
import Graphics.Haskan.Resources (alloc, alloc_, allocaAndPeek, allocaAndPeek_, peekVkList, peekVkList_)

managedRenderPass :: MonadManaged m => Vulkan.VkDevice -> Vulkan.VkSurfaceFormatKHR -> m Vulkan.VkRenderPass
managedRenderPass dev surfaceFormat = alloc "RenderPass"
  (createRenderPass dev surfaceFormat)
  (\ptr -> Vulkan.vkDestroyRenderPass dev ptr Vulkan.vkNullPtr)

createRenderPass :: MonadIO m => Vulkan.VkDevice -> Vulkan.VkSurfaceFormatKHR -> m Vulkan.VkRenderPass
createRenderPass dev surfaceFormat =
  let
    imageFormat = Vulkan.getField @"format" surfaceFormat
    colorAttachment = Vulkan.createVk
      (  set @"format" imageFormat
      &* set @"samples" Vulkan.VK_SAMPLE_COUNT_1_BIT
      &* set @"loadOp" Vulkan.VK_ATTACHMENT_LOAD_OP_CLEAR
      &* set @"storeOp" Vulkan.VK_ATTACHMENT_STORE_OP_STORE
      &* set @"stencilLoadOp" Vulkan.VK_ATTACHMENT_LOAD_OP_DONT_CARE
      &* set @"stencilStoreOp" Vulkan.VK_ATTACHMENT_STORE_OP_DONT_CARE
      &* set @"initialLayout" Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
      &* set @"finalLayout" Vulkan.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
      )
    colorAttachmentRef = Vulkan.createVk
      (  set @"attachment" 0
      &* set @"layout" Vulkan.VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
      )
    subpass = Vulkan.createVk
      (  set @"pipelineBindPoint" Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS
      &* set @"colorAttachmentCount" 1
      &* setListRef @"pColorAttachments" [colorAttachmentRef]
      &* set @"inputAttachmentCount" 0
      &* setListRef @"pInputAttachments" []
      &* set @"preserveAttachmentCount" 0
      &* setListRef @"pPreserveAttachments" []
      )
    dependency = Vulkan.createVk
      (  set @"srcSubpass" Vulkan.VK_SUBPASS_EXTERNAL
      &* set @"dstSubpass" 0
      &* set @"srcStageMask" Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
      &* set @"srcAccessMask" Vulkan.VK_ZERO_FLAGS
      &* set @"dstStageMask" Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
      &* set @"dstAccessMask" Vulkan.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
      )
    renderPassCI = Vulkan.createVk
      (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
      &* set @"pNext" Vulkan.VK_NULL
      &* set @"attachmentCount" 1
      &* setListRef @"pAttachments" [colorAttachment]
      &* set @"subpassCount" 1
      &* setListRef @"pSubpasses" [subpass]
      &* set @"dependencyCount" 1
      &* setListRef @"pDependencies" [dependency]
      )
    in liftIO $ allocaAndPeek (Vulkan.vkCreateRenderPass dev (Vulkan.unsafePtr renderPassCI) Vulkan.VK_NULL)

withRenderPass
  :: MonadIO m
  => Vulkan.VkCommandBuffer
  -> Vulkan.VkRenderPass
  -> Vulkan.VkFramebuffer
  -> Vulkan.VkExtent2D
  -> m a
  -> m a
withRenderPass commandBuffer renderPass framebuffer extent action =
  let
    blue = Vulkan.createVk
      (  setAt @"float32" @0 0.0
      &* setAt @"float32" @1 0.0
      &* setAt @"float32" @2 1.0
      &* setAt @"float32" @3 1.0
      )
    clearColorValue = Vulkan.createVk (set @"color" blue)
    offset = Vulkan.createVk
      (  set @"x" 0
      &* set @"y" 0
      )
    renderArea = Vulkan.createVk
      (  set @"offset" offset
      &* set @"extent" extent
      )
    beginInfo = Vulkan.createVk
      (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
      &* set @"pNext" Vulkan.vkNullPtr
      &* set @"renderPass" renderPass
      &* set @"framebuffer" framebuffer
      &* set @"renderArea" renderArea
      &* set @"clearValueCount" 1
      &* setListRef @"pClearValues" [clearColorValue]
      )
    begin = liftIO $ Vulkan.vkCmdBeginRenderPass commandBuffer (Vulkan.unsafePtr beginInfo) Vulkan.VK_SUBPASS_CONTENTS_INLINE
    end = liftIO $ Vulkan.vkCmdEndRenderPass commandBuffer

  in (begin *> action <* end)
