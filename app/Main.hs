{-# language OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import qualified Graphics.Haskan as Haskan

main :: IO ()
main = do
  modelName <- head <$> getArgs
  print ("Loading model: " <> modelName)
{-
  let
    scene = Scene { camera = defaultOrbitalCamera
                  , models = [modelName]
                  }
-}
  Haskan.runHaskan "Haskan Demo" modelName
