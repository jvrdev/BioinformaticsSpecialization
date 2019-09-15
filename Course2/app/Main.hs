module Main where

import qualified Data.Text.IO as T.IO
import Lib

main :: IO ()
main = runOverlapGraph "dataset_198_10.txt"
  --T.IO.putStrLn y
  
