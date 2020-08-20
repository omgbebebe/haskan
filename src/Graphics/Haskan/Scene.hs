module Graphics.Haskan.Scene where

import Data.Text (Text)
import qualified Data.Graph.Dynamic.Levels as GD
import qualified Data.Tree as T

import Linear.Matrix (M44(..), (!*!))

type Shader = String

data Viewport
  = Viewport { vpPostProcessing :: Maybe Shader
             , vpWidth :: Int
             , vpHeight :: Int
             } deriving Show

data Node
  = Node
  | Object
  | Node2D
  | Node3D

data Scene
  = Scene {}

class IsObject a where
  transformations :: a -> M44 Float

class IsNode a where
  nodeName :: a -> Text

class HasViewport s where
  viewport :: s -> Viewport

renderScene :: (HasViewport s) => s -> IO ()
renderScene s = do
  print $ "render scene with viewport " <> show (viewport s)
 
gmain :: IO ()
gmain = do
    graph <- GD.empty' @String
    mapM_ (GD.insert_ graph) ["Akanu", "Kanoa", "Kekoa", "Kaiwi", "Onakea"]
    GD.link_ graph "Akanu" "Kanoa"
    GD.link_ graph "Akanu" "Kaiwi"
    GD.link_ graph "Akanu" "Onakea"
    GD.link_ graph "Kaiwi" "Onakea"
    GD.link_ graph "Onakea" "Kanoa"
    GD.link_ graph "Kanoa" "Kekoa"

    GD.connected graph "Kaiwi" "Kekoa" >>= print
    GD.cut_ graph "Kaiwi" "Akanu"
    GD.cut_ graph "Onakea" "Akanu"
    GD.cut_ graph "Onakea" "Kanoa"
    GD.connected graph "Kaiwi" "Kekoa" >>= print
    GD.link_ graph "Akanu" "Kaiwi"
    GD.connected graph "Kaiwi" "Kekoa" >>= print

