module Graphics.Haskan where

-- text
import Data.Text (Text)

-- haskan
import Graphics.Haskan.Engine (EngineConfig(..))
import qualified Graphics.Haskan.Engine as Engine
import Graphics.Haskan.Logger (logI)

data QueueFamily
  = Graphics
  | Compute
  | Transfer
  | Sparse

--init :: MonadIO m => Text -> m ()
runHaskan :: Text -> IO ()
runHaskan title = do
  logI "Initialize Haskan Engine"
  logI "Starting Engine main loop"
  Engine.mainLoop
    EngineConfig{ targetRenderFPS = 60
                , targetPhysicsFPS = 300
                , targetNetworkFPS = 10
                , title = title
                }
  logI "Haskan finished"

