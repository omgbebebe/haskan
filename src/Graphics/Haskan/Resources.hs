{-# language RankNTypes #-}
module Graphics.Haskan.Resources
  ( alloc
  , alloc_
  , MonadManaged
  , allocaAndPeek
  , allocaAndPeek_
  , allocaAndPeekVkResult
  , peekVkList
  , peekVkList_
  , throwVkResult
  ) where

-- base
import Control.Monad.IO.Class (MonadIO)
import Control.Exception (bracket)
import qualified Foreign.Marshal.Alloc
import qualified Foreign.Marshal.Array
import           Foreign.Storable (Storable, peek)

-- text
import Data.Text (Text)
import qualified Data.Text.IO as T

-- managed
import Control.Monad.Managed (MonadManaged, using, managed)

-- vulkan-api
import qualified Graphics.Vulkan.Core_1_0 as Vulkan

alloc :: MonadManaged m => Text -> IO a -> (a -> IO b) -> m a
alloc resName create destroy =
  using
  ( managed
    ( bracket
      (T.putStrLn ("allocate " <> resName) *> create)
      (\r -> T.putStrLn ("deallocate " <> resName) *> destroy r)
    )
  )

alloc_ :: MonadManaged m => Text -> IO a -> IO b -> m a
alloc_ resName create destroy = alloc resName create (\_ -> destroy)

allocaAndPeek :: Storable a => (Vulkan.Ptr a -> IO Vulkan.VkResult) -> IO a
allocaAndPeek f = Foreign.Marshal.Alloc.alloca (\ptr -> (f ptr >>= throwVkResult) *> Foreign.Storable.peek ptr)

allocaAndPeekVkResult :: Storable a => (Vulkan.Ptr a -> IO Vulkan.VkResult) -> IO (a, Vulkan.VkResult)
allocaAndPeekVkResult f = Foreign.Marshal.Alloc.alloca $ \ptr -> do
  res <- f ptr
  d <- Foreign.Storable.peek ptr
  pure (d, res)

allocaAndPeek_ :: Storable a => (Vulkan.Ptr a -> IO ()) -> IO a
allocaAndPeek_ f = Foreign.Marshal.Alloc.alloca (\ptr -> f ptr *> Foreign.Storable.peek ptr)

peekVkList
  :: (Storable a, Integral a, Storable b)
  => (Vulkan.Ptr a -> Vulkan.Ptr b -> IO Vulkan.VkResult) -> IO [b]
peekVkList vkGetList = do
  Foreign.Marshal.Alloc.alloca $ \pCount -> do
    vkGetList pCount Vulkan.VK_NULL >>= throwVkResult
    count <- Foreign.Storable.peek pCount
    Foreign.Marshal.Array.allocaArray (fromIntegral count) $ \ptr -> do
      vkGetList pCount ptr >>= throwVkResult
      Foreign.Marshal.Array.peekArray (fromIntegral count) ptr

peekVkList_
  :: (Storable a, Integral a, Storable b)
  => (Vulkan.Ptr a -> Vulkan.Ptr b -> IO ()) -> IO [b]
peekVkList_ vkGetList = do
  Foreign.Marshal.Alloc.alloca $ \pCount -> do
    vkGetList pCount Vulkan.VK_NULL
    count <- Foreign.Storable.peek pCount
    Foreign.Marshal.Array.allocaArray (fromIntegral count) $ \ptr -> do
      vkGetList pCount ptr
      Foreign.Marshal.Array.peekArray (fromIntegral count) ptr

throwVkResult :: (MonadFail m, MonadIO m) => Vulkan.VkResult -> m ()
throwVkResult Vulkan.VK_SUCCESS =
  return ()
throwVkResult res =
  fail ( show res )
