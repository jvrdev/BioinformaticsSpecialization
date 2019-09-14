module Main where

import qualified Data.Text.IO as T.IO
import Lib

main :: IO ()
main = do
  y <- runSpelledKmersToGenome "dataset_198_3.txt"
  T.IO.putStrLn y
  
