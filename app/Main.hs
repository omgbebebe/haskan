{-# language OverloadedStrings #-}
module Main where

import qualified Graphics.Haskan as Haskan

main :: IO ()
main = do
  Haskan.runHaskan "Haskan Demo"
