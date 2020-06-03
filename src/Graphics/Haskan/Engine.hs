module Graphics.Haskan.Engine where

-- base
import Control.Monad.IO.Class (MonadIO)

-- haskan
--import Graphics.Haskan.Events (EventsQueue)
import Graphics.Haskan.Vulkan.Render (RenderContext)

--mainLoop :: MonadIO m => EventsQueue -> RenderContext -> m ()
--mainLoop eventsQueue ctx@RenderContext{..} = do
mainLoop :: MonadIO m => RenderContext -> m ()
mainLoop ctx = do
--  drawFrame ctx
  mainLoop ctx
