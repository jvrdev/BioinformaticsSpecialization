module Main where

import qualified Data.Text.IO as T.IO
import Lib

main :: IO ()
main = do
  kmersResult <- runKmersOnFile "dataset_197_3.txt"
  T.IO.putStrLn kmersResult
  
