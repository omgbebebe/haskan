module Graphics.Haskan.Vertex where

-- base
import Data.Word (Word8)
import qualified Foreign.C

-- linear
import Linear (V2(..), V3(..), V4(..))

-- haskan
import Graphics.Haskan.Vulkan.VertexFormat

type Vertex = V2 (V3 Foreign.C.CFloat)

{-
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
