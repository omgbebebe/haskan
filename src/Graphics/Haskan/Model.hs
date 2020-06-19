module Graphics.Haskan.Model where

-- base
import Data.Maybe (fromMaybe, fromJust)
import Data.Word (Word32)
import Data.List (elemIndex, sortBy, sort, concatMap)
import qualified Foreign.C
import Debug.Trace

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

verts :: [V3 Float]
verts =
  [V3 (-1.0)   1.0   (-1.0)
  ,V3 (-1.0) (-1.0)  (-1.0)
  ,V3   1.0    1.0   (-1.0)
  ,V3   1.0  (-1.0)  (-1.0)

  ,V3 (-1.0)   1.0   (1.0)
  ,V3 (-1.0) (-1.0)  (1.0)
  ,V3   1.0    1.0   (1.0)
  ,V3   1.0  (-1.0)  (1.0)
  ]
indxs :: [Int]
indxs =
  [0,1,2, 3,2,1 -- front
  ,0,2,4, 6,4,2 -- top
  ,2,3,6, 7,6,2 -- right
  ,5,1,7, 3,7,1 -- bottom
  ,0,1,5, 4,0,5 -- left
  ,4,6,5, 7,5,6 -- back
  ]

--normalizeMesh :: Mesh -> Mesh
--normalizeMesh (Mesh vertices indices) =
--normalizeMesh :: [V3 Float] -> [Int] -> [(V3 Float, V3 Float, V3 Float)]
newtype Face a = Face {unFace :: (Int, Int, Int)}

instance Eq a => Eq (Face a) where
  (==) (Face (a1,b1,c1)) (Face (a2,b2,c2)) =
    (a1 == a2 && b1 == b2 && c1 == c2) ||
    (a1 == b2 && b1 == c2 && c1 == a2) ||
    (a1 == c2 && b1 == a2 && c1 == b2)

instance Ord a => Ord (Face a) where
  compare (Face f1) (Face f2) = f1 `compare` f2

instance Show a => Show (Face a) where
  show (Face x) = show x

test :: (Show a, Eq a, Ord a) => ([Face a], [Int])
test = removeDoubles $ facesPerturb $ normalizeMesh verts indxs

facesPerturb faces = sort $ concatMap (
  \(Face (a, b, c)) ->
    [Face (a,b,c), Face (b,c,a), Face (c,a,b)]
  ) faces

removeDoubles :: (Show a, Eq a) => [Face a] -> ([Face a], [Int])
removeDoubles =
  foldr (
  \f@(Face (a,b,c)) (seen,fsts) ->
--    trace (show (a,b,c) <> ": " <> show seen <> " " <> show fsts) $
    if f `elem` seen || a `elem` fsts
    then
      (seen,fsts)
    else
      (f : seen, a : fsts)
  ) ([], [])
 
normalizeMesh vertices indices =
  let
    stride (a:b:c:xs) = ((a,b,c) : stride xs)
    stride [] = []
    stride _ = fail "index list must contain triplets"
    faces = map (
      \(a,b,c) ->
        let minIndex = fromJust (elemIndex (minimum [a,b,c]) [a,b,c])
            (f,s,t) = case minIndex of
              0 -> (a,b,c)
              1 -> (b,c,a)
              2 -> (c,a,b)
        in (f,s,t) --(vertices !! f, vertices !! s, vertices !! t)
      ) (stride indices)
--    normals = map (\(a,b,c) -> calcNormal a b c) faces
  in Face <$> faces

normalizeIndices indices =
 map (
  \(a,b,c) ->
    let minIndex = fromJust (elemIndex (minimum [a,b,c]) [a,b,c])
        (f,s,t) = case minIndex of
          0 -> (a,b,c)
          1 -> (b,c,a)
          2 -> (c,a,b)
    in (f,s,t) --(vertices !! f, vertices !! s, vertices !! t)
  ) indices

rotateFace n (a, b, c) =
  case n of
    1 -> (b, c, a)
    2 -> (c, a, b)
    _ -> (a, b, c)

normFaces faces =
  let
--    sorted = sortBy compareFst3 faces
    faces' = sortBy compareFst3 (normPass faces)
  in
    if isNormalized faces'
    then
      faces'
    else
      sortBy compareFst3 $ normalizeIndices faces'

fst3 (a,_,_) = a
compareFst3 a b = compare (fst3 a) (fst3 b)

normalizePair (a, b) =
  case compareFst3 a b of
    EQ -> (a, rotateFace 1 b)
    _ -> (a, b)

normPass faces = scanl (\b a -> if (compareFst3 a b) == EQ then rotateFace 1 a else a) (head faces) (tail faces)

isNormalized faces = all id $ isNormalized' faces

isNormalized' :: [(Int, Int, Int)] -> [Bool]
isNormalized' (a:b:rest) = (EQ /= (compareFst3 a b)):isNormalized' (b:rest)
isNormalized' (a:[]) = []
isNormalized' [] = []
