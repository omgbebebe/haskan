module Graphics.Haskan.Vulkan.Buffer where

-- base
import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Traversable (for)
import qualified Foreign
import qualified Foreign.Marshal
import qualified Foreign.Ptr
import Foreign.Storable (Storable)
import qualified Foreign.Storable
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

-- pretty-simple
import Text.Pretty.Simple

-- haskan
import Graphics.Haskan.Vertex (Vertex)
import qualified Graphics.Haskan.Vulkan.Memory as Memory
import Graphics.Haskan.Resources (alloc, alloc_, allocaAndPeek, allocaAndPeek_, peekVkList, peekVkList_, throwVkResult)

managedBuffer
  :: (MonadManaged m, Storable a)
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> [a]
  -> (Vulkan.VkBufferUsageBitmask Vulkan.FlagMask)
  -> m Vulkan.VkBuffer
managedBuffer pdev dev data' usage = alloc "Buffer"
  (createBuffer pdev dev data' usage)
  (\ptr -> Vulkan.vkDestroyBuffer dev ptr Vulkan.vkNullPtr)

createBuffer
  :: (MonadFail m, MonadIO m, Storable a)
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> [a]
  -> (Vulkan.VkBufferUsageBitmask Vulkan.FlagMask)
  -> m Vulkan.VkBuffer
createBuffer pdev dev data' usage = do
  let
    size = fromIntegral ((length data') * Foreign.sizeOf ( undefined :: Vertex))
    createInfo = Vulkan.createVk
      (  set @"sType" Vulkan.VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
      &* set @"pNext" Vulkan.VK_NULL
      &* set @"usage" usage
      &* set @"size" size
      &* set @"sharingMode" Vulkan.VK_SHARING_MODE_EXCLUSIVE
      &* set @"queueFamilyIndexCount" 0
      &* set @"pQueueFamilyIndices" Vulkan.VK_NULL
      )
  buffer <- allocaAndPeek (Vulkan.vkCreateBuffer dev (Vulkan.unsafePtr createInfo) Vulkan.vkNullPtr)
  pPrint buffer
  memoryRequirements <- allocaAndPeek_ (Vulkan.vkGetBufferMemoryRequirements dev buffer)
  pPrint memoryRequirements
  memory <-
    -- TODO: VkMemory MUST be managed
    Memory.allocateMemoryFor
    pdev
    dev
    memoryRequirements
    [ Vulkan.VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
    , Vulkan.VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
    ]

  liftIO $ do
    Vulkan.vkBindBufferMemory dev buffer memory 0 {- offset-} >>= throwVkResult
    memPtr <-
      allocaAndPeek (Vulkan.vkMapMemory dev memory 0 size Vulkan.VK_ZERO_FLAGS)
    Foreign.Marshal.pokeArray (Foreign.castPtr memPtr) data'
    Vulkan.vkUnmapMemory dev memory
   
  pure buffer

managedVertexBuffer :: MonadManaged m => Vulkan.VkPhysicalDevice -> Vulkan.VkDevice -> [Vertex] -> m Vulkan.VkBuffer
managedVertexBuffer pdev dev vertices = managedBuffer pdev dev vertices Vulkan.VK_BUFFER_USAGE_VERTEX_BUFFER_BIT

--managedIndexBuffer :: MonadManaged m => Vulkan.VkPhysicalDevice -> Vulkan.VkDevice -> m Vulkan.VkBuffer
--managedIndexBuffer pdev dev = managedBuffer pdev dev [] Vulkan.VK_BUFFER_USAGE_INDEX_BUFFER_BIT