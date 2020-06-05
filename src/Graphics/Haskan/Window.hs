module Graphics.Haskan.Window where

-- base
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce

-- text
import Data.Text (Text)

-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- managed
import Control.Monad.Managed (MonadManaged)

-- sdl2
import qualified SDL
import qualified SDL.Video.Vulkan

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Ext as Vulkan

-- haskan
import Graphics.Haskan.Resources (alloc, alloc_)
import Graphics.Haskan.Logger (logI, showT)

managedWindow
  :: MonadManaged m
  => Text
  -> (Int, Int)
  -> m ([ByteString], SDL.Window)
managedWindow title (width, height) = do
  SDL.initialize @[] [SDL.InitVideo]
  alloc_ "Vulkan library"
    (SDL.Video.Vulkan.vkLoadLibrary Nothing)
    SDL.Video.Vulkan.vkUnloadLibrary
  window <- alloc "SDL window"
    (SDL.createWindow
      title
      (SDL.defaultWindow
       { SDL.windowInitialSize =
           SDL.V2 (fromIntegral width) (fromIntegral height)
       , SDL.windowGraphicsContext = SDL.VulkanContext
       , SDL.windowResizable       = True
       , SDL.windowHighDPI         = True
       , SDL.windowVisible         = False
       }
      )
    )
    SDL.destroyWindow

  windowExtensions <-
    liftIO $ traverse BS.packCString =<< SDL.Video.Vulkan.vkGetInstanceExtensions window
  logI ("Window extensions: " <> showT windowExtensions)
  pure (windowExtensions, window)

managedSurface
  :: MonadManaged m
  => Vulkan.VkInstance
  -> SDL.Window
  -> m Vulkan.VkSurfaceKHR
managedSurface inst window = alloc "Surface"
  (Vulkan.VkPtr <$> SDL.Video.Vulkan.vkCreateSurface window (coerce inst))
  (\ptr -> Vulkan.vkDestroySurfaceKHR (coerce inst) ptr Vulkan.vkNullPtr)

showWindow :: MonadIO m => SDL.Window -> m ()
showWindow window = liftIO (SDL.showWindow window)
