module Lib
    ( DnaString(..)
    , slice
    , kmers
    , spelledKmersToGenome
    , overlapGraph
    , runKmersOnFile
    , runSpelledKmersToGenome
    , runOverlapGraph
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

data DnaString = DnaString T.Text deriving (Show, Eq)
type AdjacencyList a = [(a, [a])]

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

connectsWith :: DnaString -> DnaString -> Bool
connectsWith (DnaString a) (DnaString b) = (T.drop 1 a) == (T.dropEnd 1 b)

overlapGraph :: [DnaString] -> AdjacencyList DnaString
overlapGraph xs =
  filter sndIsNotEmpty $ map adjacencyEntry $ xs
  where
    adjacencyEntry a = (a, filter (connectsWith a) xs)
    sndIsNotEmpty = not . null . snd 

runKmersOnFile :: FilePath -> IO T.Text
runKmersOnFile file = do
  content <- T.IO.readFile file
  let l0:l1:_ = T.lines content
  let k = (read :: String -> Int) (T.unpack l0)
  let dna = DnaString l1
  let kmersResult = kmers k dna
  return $ T.unlines $ map dnaStringPrint kmersResult

runSpelledKmersToGenome :: FilePath -> IO T.Text
runSpelledKmersToGenome file = do
  content <- T.IO.readFile file
  let dnas = map DnaString $ T.lines content
  return $
    case spelledKmersToGenome dnas of
      Just (DnaString x) -> x
      Nothing -> T.pack ""

runOverlapGraph :: FilePath -> IO T.Text
runOverlapGraph file = do
  return $ T.pack ""
