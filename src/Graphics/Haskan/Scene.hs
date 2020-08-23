{-# language TypeFamilies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TemplateHaskell #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
module Graphics.Haskan.Scene where

-- base
import System.Random
--import Data.Monoid
import Control.Monad

-- text
import Data.Text (Text)

-- synamic-graphs
import qualified Data.Graph.Dynamic.Levels as GD
import qualified Data.Tree as T

-- apecs
import Apecs

-- linear
import Linear (V2(..), norm, (*^))
import Linear.Matrix (M44(..), (!*!))
import qualified Linear.Matrix as Matrix

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

-- apecs tests
newtype Position = Position (V2 Float) deriving Show
instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity (V2 Float) deriving Show
instance Component Velocity where type Storage Velocity = Map Velocity

data Target = Target deriving Show
instance Component Target where type Storage Target = Map Target

data Bullet = Bullet deriving Show
instance Component Bullet where type Storage Bullet = Map Bullet

data Particle = Particle Float deriving Show
instance Component Particle where type Storage Particle = Map Particle

data Player = Player deriving Show
instance Component Player where type Storage Player = Unique Player

newtype Score = Score Int deriving (Show, Num)
instance Semigroup Score where (<>) = (+)
instance Monoid Score where mempty = 0
instance Component Score where type Storage Score = Global Score

newtype Time = Time Float deriving (Show, Num)
instance Semigroup Time where (<>) = (+)
instance Monoid Time where mempty = 0
instance Component Time where type Storage Time = Global Time

makeWorld "World" [''Position, ''Velocity, ''Player, ''Target, ''Bullet, ''Score, ''Time, ''Particle]

type System' a = System World a
type Kinetic = (Position, Velocity)

-- ECS Scene model
newtype Transformation3D = Transformation3D (M44 Float) deriving Show
instance Component Transformation3D where type Storage Transformation3D = Map Transformation3D

data Opaque = Opaque deriving Show
instance Component Opaque where type Storage Opaque = Map Opaque

data Camera = Camera deriving Show
instance Component Camera where type Storage Camera = Unique Camera

makeWorld "Scene3D" [''Time, ''Camera, ''Transformation3D, ''Opaque]

type SystemScene3D' a = System Scene3D a

initializeScene3D :: SystemScene3D' ()
initializeScene3D = do
  let
    camTrans = Matrix.identity
   
  camera <- newEntity (Camera, Transformation3D camTrans)
  return ()


drawScene3D :: SystemScene3D' ()
drawScene3D = do
  cmapM_ $ \(Camera, Transformation3D trans3D) -> do
    liftIO . print $ "Camera trans: " <> show trans3D
-- end ECS Scene model

playerSpeed, bulletSpeed, enemySpeed, xmin, xmax :: Float
playerSpeed = 170
bulletSpeed = 500
enemySpeed  = 80
xmin = -100
xmax = 100

hitBonus, missPenalty :: Int
hitBonus = 100
missPenalty = 40

playerPos, scorePos :: V2 Float
playerPos = V2 0 (-120)
scorePos  = V2 xmin (-170)

initialize :: System' ()
initialize = do
  playerEty <- newEntity (Player, Position playerPos, Velocity 0)
  return ()

stepPosition :: Float -> System' ()
stepPosition dT = cmap $ \(Position p, Velocity v) -> Position (p + dT *^ v)

clampPlayer :: System' ()
clampPlayer = cmap $ \(Player, Position (V2 x y))
                   -> Position (V2 (min xmax . max xmin $ x) y)

incrTime :: Float -> System' ()
incrTime dT = modify global $ \(Time t) -> Time (t+dT)

clearTargets :: System' ()
clearTargets = cmap $ \all@(Target, Position (V2 x _), Velocity _) ->
  if x < xmin || x > xmax
     then Nothing
     else Just all

stepParticles :: Float -> System' ()
stepParticles dT = cmap $ \(Particle t) ->
  if t < 0
     then Right $ Not @(Particle, Kinetic)
     else Left  $ Particle (t-dT)


clearBullets :: System' ()
clearBullets = cmap $ \(Bullet, Position (V2 _ y), Score s) ->
  if y > 170
     then Right $ (Not @(Bullet, Kinetic), Score (s-missPenalty))
     else Left ()

handleCollisions =
  cmapM_ $ \(Target, Position posT, etyT) ->
    cmapM_ $ \(Bullet, Position posB, etyB) ->
      when (norm (posT - posB) < 10) $ do
        destroy etyT (Proxy @(Target, Kinetic))
        destroy etyB (Proxy @(Bullet, Kinetic))
        spawnParticles 15 (Position posB) (-500,500) (200,-50)
        modify global $ \(Score x) -> Score (x + hitBonus)


triggerEvery :: Float -> Float -> Float -> System' a -> System' ()
triggerEvery dT period phase sys = do
  Time t <- get global
  let t' = t + phase
      trigger = floor (t'/period) /= floor ((t'+dT)/period)
  when trigger $ void sys

spawnParticles :: Int -> Position -> (Float,Float) -> (Float,Float) -> System' ()
spawnParticles n pos dvx dvy = replicateM_ n $ do
  vx <- liftIO $ randomRIO dvx
  vy <- liftIO $ randomRIO dvy
  t  <- liftIO $ randomRIO (0.02,0.3)
  newEntity (Particle t, pos, Velocity (V2 vx vy))

step :: Float -> System' ()
step dT = do
  incrTime dT
  stepPosition dT
  clampPlayer
  clearTargets
  clearBullets
  stepParticles dT
  handleCollisions
  triggerEvery dT 0.6 0   $ newEntity (Target, Position (V2 xmin 80), Velocity (V2 enemySpeed 0))
  triggerEvery dT 0.6 0.3 $ newEntity (Target, Position (V2 xmax 120), Velocity (V2 (negate enemySpeed) 0))
-- end apecs tests

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

