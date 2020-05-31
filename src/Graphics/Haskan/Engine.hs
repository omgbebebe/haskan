module Graphics.Haskan.Engine where

-- base
import Control.Monad.IO.Class (MonadIO, liftIO)

-- haskan
--import Graphics.Haskan.Events (EventsQueue)
import Graphics.Haskan.Vulkan.Render (RenderContext(..), drawFrame)

--mainLoop :: MonadIO m => EventsQueue -> RenderContext -> m ()
--mainLoop eventsQueue ctx@RenderContext{..} = do
mainLoop :: MonadIO m => RenderContext -> m ()
mainLoop ctx@RenderContext{..} = do
--  drawFrame ctx
  mainLoop ctx
