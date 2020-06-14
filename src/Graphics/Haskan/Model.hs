module Graphics.Haskan.Model where

-- base
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import qualified Foreign.C
-- linear
import Linear (V2(..), V3(..), V4(..), normalize)

-- haskan
import Graphics.Haskan.Vertex (Vertex(..))
import Graphics.Haskan.Utils.PieLoader (PieLevel(..))
import qualified Graphics.Haskan.Utils.PieLoader as Pie

data Mesh =
  Mesh { vertices :: [Vertex]
       , indices :: [Word32]
       } deriving (Eq, Show)

fromPie :: PieLevel -> Mesh
fromPie PieLevel{..} = do
  let indices = concatMap
        (\t ->
           let (V3 a b c) = Pie.indices t
           in [fromIntegral c, fromIntegral b, fromIntegral a] -- reverse to counterclockwise
        ) triangles
      normals = concatMap
        (\t ->
           let (V3 a b c) = Pie.indices t
               norm = calcNormal (vertices !! a) (vertices !! b) (vertices !! c)
           in [(a, norm), (b, norm), (c, norm)]
        ) triangles
      verts = map
        (\(ndx, (V3 x y z)) ->
           Vertex {vPos = V3 (realToFrac x) (realToFrac y) (realToFrac z)
                  ,vTexUV = V2 0.0 0.0
                  ,vNorm = (fromMaybe (V3 0.0 1.0 0.0) (lookup ndx normals))
                  ,vCol = V4 155 155 0 255
                  }
        ) (zip [0..] vertices)
  Mesh verts indices

calcNormal :: V3 Float -> V3 Float -> V3 Float -> V3 Foreign.C.CFloat
calcNormal p1 p2 p3 =
  let
    (V3 ux uy uz) = p2 - p1
    (V3 vx vy vz) = p3 - p1
    nx = (ux * vz) - (uz * vy)
    ny = (uz * vx) - (ux * vz)
    nz = (ux * vy) - (uy * vx)
  in normalize $ V3 (realToFrac nx) (realToFrac ny) (realToFrac nz)
