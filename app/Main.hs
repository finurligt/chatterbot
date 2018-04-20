module Main where

import Lib

main :: IO ()
main = do
  print $ substitute 'x' "3*cos(x) + 4 - x" "5.37"
