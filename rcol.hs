module Main where

import Control.Monad
import System.Console.ANSI
import System.Random (randomRIO)

main = do 
  contents <- getContents
  mapM_ putColorChar contents
  setSGR [Reset]  

pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs -1)

putColorChar :: Char -> IO ()
putColorChar c = do
  let colors = enumFromTo minBound maxBound :: [Color]
  let intensities = [Dull, Vivid]
  color <- pick colors
  intensity <- pick intensities
  -- let layers = [Foreground, Background]
  -- layer <- pick layers
  setSGR [SetColor Foreground intensity color]
  putChar c
