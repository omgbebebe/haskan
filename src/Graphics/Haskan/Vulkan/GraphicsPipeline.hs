module Graphics.Haskan.Vulkan.GraphicsPipeline where

-- base
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits ((.|.))
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
import Graphics.Vulkan.Marshal.Create (set, setAt, setVkRef, setListRef, setStrRef, setStrListRef, (&*))

-- haskan
import Graphics.Haskan.Resources (alloc, alloc_, allocaAndPeek, allocaAndPeek_, peekVkList, peekVkList_)

managedGraphicsPipeline
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkSurfaceFormatKHR
  -> Vulkan.VkPipelineLayout
  -> Vulkan.VkRenderPass
  -> Vulkan.VkShaderModule
  -> Vulkan.VkShaderModule
  -> Vulkan.VkExtent2D
  -> m Vulkan.VkPipeline
managedGraphicsPipeline dev surfaceFormat layout renderPass vertShader fragShader swapchainExtent = alloc "GraphicsPipeline"
  (createGraphicsPipeline dev surfaceFormat layout renderPass vertShader fragShader swapchainExtent)
  (\ptr -> Vulkan.vkDestroyPipeline dev ptr Vulkan.vkNullPtr)

createGraphicsPipeline
  :: MonadIO m
  => Vulkan.VkDevice
  -> Vulkan.VkSurfaceFormatKHR
  -> Vulkan.VkPipelineLayout
  -> Vulkan.VkRenderPass
  -> Vulkan.VkShaderModule
  -> Vulkan.VkShaderModule
  -> Vulkan.VkExtent2D
  -> m Vulkan.VkPipeline
createGraphicsPipeline dev surfaceFormat layout renderPass vertShader fragShader swapchainExtent = do
  let
    swapchainImageFormat = Vulkan.getField @"format" surfaceFormat
    vertStage = Vulkan.createVk
      (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
      &* set @"pNext" Vulkan.VK_NULL
      &* set @"stage" Vulkan.VK_SHADER_STAGE_VERTEX_BIT
      &* set @"module" vertShader
      &* setStrRef @"pName" "main"
      )
    fragStage = Vulkan.createVk
      (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
      &* set @"pNext" Vulkan.VK_NULL
      &* set @"stage" Vulkan.VK_SHADER_STAGE_FRAGMENT_BIT
      &* set @"module" fragShader
      &* setStrRef @"pName" "main"
      )
    -- stages =
    vertexInputStateCI = Vulkan.createVk
      (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
      &* set @"pNext" Vulkan.VK_NULL
      &* set @"vertexBindingDescriptionCount" 0
      &* set @"pVertexBindingDescriptions" Vulkan.VK_NULL
      &* set @"vertexAttributeDescriptionCount" 0
      &* set @"pVertexAttributeDescriptions" Vulkan.VK_NULL
      )
    assemblyInputStateCI = Vulkan.createVk
      (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
      &* set @"pNext" Vulkan.VK_NULL
      &* set @"topology" Vulkan.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
      &* set @"primitiveRestartEnable" Vulkan.VK_FALSE
      )
    tessellationState = Vulkan.VK_NULL

    viewport = Vulkan.createVk
      (  set @"x" 0
      &* set @"y" 0
      &* set @"width" (fromIntegral (Vulkan.getField @"width" swapchainExtent))
      &* set @"height" (fromIntegral (Vulkan.getField @"height" swapchainExtent))
      &* set @"minDepth" 0.0
      &* set @"maxDepth" 1.0
      )
    scissor = let
      offset = Vulkan.createVk
        (  set @"x" 0
        &* set @"y" 0
        )
      in Vulkan.createVk
         (  set @"offset" offset
         &* set @"extent" swapchainExtent
         )
    viewportState = Vulkan.createVk
      (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
      &* set @"pNext" Vulkan.VK_NULL
      &* set @"viewportCount" 1
      &* setListRef @"pViewports" [viewport]
      &* set @"scissorCount" 1
      &* setListRef @"pScissors" [scissor]
      )

    rasterizationState = Vulkan.createVk
      (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
      &* set @"pNext" Vulkan.VK_NULL
      &* set @"depthClampEnable" Vulkan.VK_FALSE
      &* set @"rasterizerDiscardEnable" Vulkan.VK_FALSE
      &* set @"polygonMode" Vulkan.VK_POLYGON_MODE_FILL
      &* set @"lineWidth" 1.0
      &* set @"cullMode" Vulkan.VK_CULL_MODE_BACK_BIT
      &* set @"frontFace" Vulkan.VK_FRONT_FACE_CLOCKWISE
      &* set @"depthBiasEnable" Vulkan.VK_FALSE
      &* set @"depthBiasConstantFactor" 0.0
      &* set @"depthBiasClamp" 0.0
      &* set @"depthBiasSlopeFactor" 0.0
      )
    multisampleState = Vulkan.createVk
      (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
      &* set @"pNext" Vulkan.VK_NULL
      &* set @"sampleShadingEnable" Vulkan.VK_FALSE
      &* set @"rasterizationSamples" Vulkan.VK_SAMPLE_COUNT_1_BIT
      &* set @"minSampleShading" 1.0
      &* set @"pSampleMask" Vulkan.VK_NULL
      &* set @"alphaToCoverageEnable" Vulkan.VK_FALSE
      &* set @"alphaToOneEnable" Vulkan.VK_FALSE
      )
    depthStencilState = Vulkan.VK_NULL
    colorBlendState = let
      colorBlendAttachment = Vulkan.createVk
        (  set @"colorWriteMask"
            (   Vulkan.VK_COLOR_COMPONENT_R_BIT
            .|. Vulkan.VK_COLOR_COMPONENT_G_BIT
            .|. Vulkan.VK_COLOR_COMPONENT_B_BIT
            .|. Vulkan.VK_COLOR_COMPONENT_A_BIT
            )
        &* set @"blendEnable" Vulkan.VK_FALSE
        &* set @"srcColorBlendFactor" Vulkan.VK_BLEND_FACTOR_ONE
        &* set @"dstColorBlendFactor" Vulkan.VK_BLEND_FACTOR_ZERO
        &* set @"colorBlendOp" Vulkan.VK_BLEND_OP_ADD
        &* set @"srcAlphaBlendFactor" Vulkan.VK_BLEND_FACTOR_ONE
        &* set @"dstAlphaBlendFactor" Vulkan.VK_BLEND_FACTOR_ZERO
        &* set @"alphaBlendOp" Vulkan.VK_BLEND_OP_ADD
        )
        in Vulkan.createVk
           (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
           &* set @"pNext" Vulkan.VK_NULL
           &* set @"logicOpEnable" Vulkan.VK_FALSE
           &* set @"logicOp" Vulkan.VK_LOGIC_OP_COPY
           &* set @"attachmentCount" 1
           &* setListRef @"pAttachments" [colorBlendAttachment]
           &* setAt @"blendConstants" @0 0.0
           &* setAt @"blendConstants" @1 0.0
           &* setAt @"blendConstants" @2 0.0
           &* setAt @"blendConstants" @3 0.0
           )
  {- TODO
    dynamicState = Vulkan.createVk
      (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO
      &* set @"pNext" Vulkan.VK_NULL
      &* set @"dynamicStateCount" 1
      &* setListRef @"pDynamicStates" [Vulkan.VK_DYNAMIC_STATE_VIEWPORT]
      )
-}
    dynamicState = Vulkan.createVk
      (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO
      &* set @"pNext" Vulkan.VK_NULL
      &* set @"dynamicStateCount" 0
      &* setListRef @"pDynamicStates" []
      )
    subpass = 0
    basePipelineHandle = Vulkan.VK_NULL_HANDLE
    basePipelineIndex = (-1)
    pipelineCI = Vulkan.createVk
      (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
      &* set @"pNext" Vulkan.VK_NULL
      &* set @"flags" Vulkan.VK_ZERO_FLAGS
      &* set @"stageCount" 2
      &* setListRef @"pStages" [vertStage, fragStage]
      &* setVkRef @"pVertexInputState" vertexInputStateCI
      &* setVkRef @"pInputAssemblyState" assemblyInputStateCI
      &* set @"pTessellationState" tessellationState
      &* setVkRef @"pViewportState" viewportState
      &* setVkRef @"pRasterizationState" rasterizationState
      &* setVkRef @"pMultisampleState" multisampleState
      &* set @"pDepthStencilState" depthStencilState
      &* setVkRef @"pColorBlendState" colorBlendState
      &* setVkRef @"pDynamicState" dynamicState
      &* set @"layout" layout
      &* set @"renderPass" renderPass
      &* set @"subpass" subpass
      &* set @"basePipelineHandle" basePipelineHandle
      &* set @"basePipelineIndex" basePipelineIndex
      )
    in liftIO $ allocaAndPeek $ Vulkan.vkCreateGraphicsPipelines dev Vulkan.VK_NULL 1 (Vulkan.unsafePtr pipelineCI) Vulkan.VK_NULL

cmdBindPipeline :: MonadIO m => Vulkan.VkCommandBuffer -> Vulkan.VkPipeline -> m ()
cmdBindPipeline commandBuffer pipeline = liftIO $
  Vulkan.vkCmdBindPipeline commandBuffer Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS pipeline -- >>= throwVkResult
