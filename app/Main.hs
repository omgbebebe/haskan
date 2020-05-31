{-# language OverloadedStrings #-}
module Main where

import qualified Graphics.Haskan as Haskan

main :: IO ()
main = do
  putStrLn "Starting here..."
  Haskan.initHaskan "Haskan Demo"
  putStrLn "Hello, Haskell!"
