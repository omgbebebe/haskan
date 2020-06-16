module Graphics.Haskan.Model where

-- base
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import qualified Foreign.C
--import Debug.Trace

-- linear
import Linear (V2(..), V3(..), V4(..), normalize)

-- pretty-simple
import Debug.Pretty.Simple

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
           in [fromIntegral a, fromIntegral b, fromIntegral c] -- reverse to counterclockwise
        ) triangles
      firstNormals = map
        (\t ->
           let (V3 a b c) = Pie.indices t
               norm = calcNormal (vertices !! a) (vertices !! b) (vertices !! c)
           in (a, norm)
        ) triangles
      verts = map
        (\(i, (V3 x y z)) ->
           let
             norm = fromMaybe (V3 0.0 0.0 0.0) (lookup i firstNormals)
           in Vertex {vPos = V3 (realToFrac x) (realToFrac y) (realToFrac z)
                     ,vTexUV = V2 0.0 0.0
                     ,vNorm = pTrace (show i <> " => " <> show norm) norm
                     ,vCol = V4 155 155 0 255
                     }
        ) (zip [0..] vertices)
  Mesh verts indices

calcNormal :: V3 Float -> V3 Float -> V3 Float -> V3 Foreign.C.CFloat
calcNormal p1 p2 p3 =
  let
    (V3 ux uy uz) = p2 - p1
    (V3 vx vy vz) = p3 - p1
    nx = (uy * vz) - (uz * vy)
    ny = (uz * vx) - (ux * vz)
    nz = (ux * vy) - (uy * vx)
  in normalize $ V3 (realToFrac nx) (realToFrac ny) (realToFrac nz)
