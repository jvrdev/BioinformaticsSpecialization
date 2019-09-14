module Lib
    ( DnaString(..)
    , slice
    , kmers
    , spelledKmersToGenome
    , runKmersOnFile
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

data DnaString = DnaString T.Text deriving (Show, Eq)

dnaStringLength :: DnaString -> Int
dnaStringLength (DnaString s) = T.length s

dnaStringPrint :: DnaString -> T.Text
dnaStringPrint (DnaString s) =  s

slice :: Int -> Int -> DnaString -> DnaString
slice startIndex count (DnaString s) =
  DnaString $ T.take count $ T.drop startIndex s

kmers :: Int -> DnaString -> [DnaString]
kmers k dna =
  map kmerAt [ 0 .. dnaStringLength dna - k]
  where kmerAt i = slice i k dna

spelledKmersToGenome :: [DnaString] -> Maybe DnaString
spelledKmersToGenome [] = Nothing
spelledKmersToGenome ((DnaString h):t) =
  Just $ DnaString $ T.pack $ T.unpack h ++ map (T.last . dnaStringPrint) t

runKmersOnFile :: FilePath -> IO T.Text
runKmersOnFile file = do
  content <- T.IO.readFile file
  let l0:l1:_ = T.lines content
  let k = (read :: String -> Int) (T.unpack l0)
  let dna = DnaString l1
  let kmersResult = kmers k dna
  return $ T.unlines $ map dnaStringPrint kmersResult
  
  

