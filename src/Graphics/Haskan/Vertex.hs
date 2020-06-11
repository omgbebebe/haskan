module Graphics.Haskan.Vertex where

-- base
import Data.Word (Word8)
import qualified Foreign
import qualified Foreign.C
import Foreign.Storable (Storable(..))
-- linear
import Linear (V2(..), V3(..), V4(..))

-- haskan
import Graphics.Haskan.Vulkan.VertexFormat

type Vertex = V3 (V3 Foreign.C.CFloat)
type VertexIndex = Foreign.Word32
{-
data Vertex = Vertex{ vPos :: V3 Foreign.C.CFloat
                    , vCol :: V3 Foreign.C.CFloat
                    } deriving (Eq, Show)

data Vertex = Vertex
  { vPos :: V3 Foreign.C.CFloat
  , vColor :: V4 Word8
  } deriving Show

vertexFormat :: VertexFormat Vertex
vertexFormat =
  Vertex
  <$> v3_32sfloat
  <*> v4_8uint
-}
