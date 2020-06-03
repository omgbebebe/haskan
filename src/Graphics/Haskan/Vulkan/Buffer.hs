module Graphics.Haskan.Vulkan.Buffer where

-- base
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Foreign
import qualified Foreign.Marshal
import Foreign.Storable (Storable)

-- managed
import Control.Monad.Managed (MonadManaged)

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan
import Graphics.Vulkan.Marshal.Create (set, (&*))

-- pretty-simple
import Text.Pretty.Simple

-- haskan
import Graphics.Haskan.Vertex (Vertex, VertexIndex)
import qualified Graphics.Haskan.Vulkan.Memory as Memory
import Graphics.Haskan.Resources (alloc, allocaAndPeek, allocaAndPeek_, throwVkResult)

managedBuffer
  :: (MonadManaged m, Storable a)
  => Vulkan.VkDevice
  -> [a]
  -> (Vulkan.VkBufferUsageBitmask Vulkan.FlagMask)
  -> m (Vulkan.VkBuffer, Vulkan.VkMemoryRequirements)
managedBuffer dev data' usage = alloc "Buffer"
  (createBuffer dev data' usage)
  (\(ptr,_) -> Vulkan.vkDestroyBuffer dev ptr Vulkan.vkNullPtr)

createBuffer
  :: (MonadFail m, MonadIO m, Storable a)
  => Vulkan.VkDevice
  -> [a]
  -> (Vulkan.VkBufferUsageBitmask Vulkan.FlagMask)
  -> m (Vulkan.VkBuffer, Vulkan.VkMemoryRequirements)
createBuffer dev data' usage = do
  let
    size = fromIntegral ((length data') * (Foreign.sizeOf (head data')))
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

  pure (buffer, memoryRequirements)

createBufferMemory
  :: (MonadFail m, MonadIO m)
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> Vulkan.VkMemoryRequirements
  -> m Vulkan.VkDeviceMemory
createBufferMemory pdev dev memoryRequirements =
  Memory.allocateMemoryFor
    pdev
    dev
    memoryRequirements
    [ Vulkan.VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
    , Vulkan.VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
    ]

managedBufferMemory
  :: MonadManaged m
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> Vulkan.VkMemoryRequirements
  -> m Vulkan.VkDeviceMemory
managedBufferMemory pdev dev memoryRequirements = alloc "Buffer memory"
  (createBufferMemory pdev dev memoryRequirements)
  (\ptr -> Vulkan.vkFreeMemory dev ptr Vulkan.vkNullPtr)

bindBufferMemory
  :: (MonadIO m, Storable a)
  => Vulkan.VkDevice
  -> Vulkan.VkBuffer
  -> Vulkan.VkDeviceMemory
  -> [a]
  -> m ()
bindBufferMemory dev buffer memory data' = do
  let
    size = fromIntegral ((length data') * (Foreign.sizeOf (head data')))
  liftIO $ do
    putStrLn "bind memory"
    Vulkan.vkBindBufferMemory dev buffer memory 0 {- offset-} >>= throwVkResult
    memPtr <-
      allocaAndPeek (Vulkan.vkMapMemory dev memory 0 size Vulkan.VK_ZERO_FLAGS)
    Foreign.Marshal.pokeArray (Foreign.castPtr memPtr) data'
    Vulkan.vkUnmapMemory dev memory
    putStrLn "end bind memory"

managedVertexBuffer :: (MonadManaged m) => Vulkan.VkPhysicalDevice -> Vulkan.VkDevice -> [Vertex] -> m Vulkan.VkBuffer
managedVertexBuffer pdev dev vertices = do
  (buffer, memoryRequirements) <- managedBuffer dev vertices Vulkan.VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
  memory <- managedBufferMemory pdev dev memoryRequirements
  bindBufferMemory dev buffer memory vertices
  pure buffer

managedIndexBuffer :: (MonadManaged m) => Vulkan.VkPhysicalDevice -> Vulkan.VkDevice -> [VertexIndex] -> m Vulkan.VkBuffer
managedIndexBuffer pdev dev indices = do
  (buffer, memoryRequirements) <- managedBuffer dev indices Vulkan.VK_BUFFER_USAGE_INDEX_BUFFER_BIT
  memory <- managedBufferMemory pdev dev memoryRequirements
  bindBufferMemory dev buffer memory indices
  pure buffer

managedUniformBuffer :: (MonadManaged m, Storable a) => Vulkan.VkPhysicalDevice -> Vulkan.VkDevice -> [a] -> m Vulkan.VkBuffer
managedUniformBuffer pdev dev values = do
  (buffer, memoryRequirements) <- managedBuffer dev values Vulkan.VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
  memory <- managedBufferMemory pdev dev memoryRequirements
  bindBufferMemory dev buffer memory values
  pure buffer

--managedIndexBuffer :: MonadManaged m => Vulkan.VkPhysicalDevice -> Vulkan.VkDevice -> m Vulkan.VkBuffer
--managedIndexBuffer pdev dev = managedBuffer pdev dev [] Vulkan.VK_BUFFER_USAGE_INDEX_BUFFER_BIT
