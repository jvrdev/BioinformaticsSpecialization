module Main where

import qualified Data.Text.IO as T.IO
import Lib

main :: IO ()
main = runDeBruijnGraphFromKmers "dataset_200_8.txt"
  --T.IO.putStrLn y
  
