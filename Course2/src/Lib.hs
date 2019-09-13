module Lib
    ( someFunc
    , DnaString(..)
    , slice
    , kmers
    ) where

import qualified Data.Text as T

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data DnaString = DnaString T.Text deriving (Show, Eq)

dnaStringLength :: DnaString -> Int
dnaStringLength (DnaString s) = T.length s

slice :: Int -> Int -> DnaString -> DnaString
slice startIndex count (DnaString s) =
  DnaString $ T.take count $ T.drop startIndex s

kmers :: Int -> DnaString -> [DnaString]
kmers k dna =
  map kmerAt [ 0 .. dnaStringLength dna - k]
  where
    kmerAt i = slice i k dna

