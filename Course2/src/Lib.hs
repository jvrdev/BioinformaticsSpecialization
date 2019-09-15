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

import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

data DnaString = DnaString T.Text deriving (Eq)

instance Show DnaString where
  show (DnaString s) = T.unpack s
 
type AdjacencyList a = [(a, [a])]

dnaStringLength :: DnaString -> Int
dnaStringLength (DnaString s) = T.length s

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
  Just $ DnaString $ T.pack $ T.unpack h ++ map (last . show) t

connectsWith :: DnaString -> DnaString -> Bool
connectsWith (DnaString a) (DnaString b) = (T.drop 1 a) == (T.dropEnd 1 b)

overlapGraph :: [DnaString] -> AdjacencyList DnaString
overlapGraph xs =
  filter sndIsNotEmpty $ map adjacencyEntry $ xs
  where
    adjacencyEntry a = (a, filter (connectsWith a) xs)
    sndIsNotEmpty = not . null . snd 

printAdjacencyList :: Show a => AdjacencyList a -> IO ()
printAdjacencyList xs =
  mapM_ f xs
  where
    f (node, connects) = do
      putStr $ show node
      putStr " -> "
      mapM_ putStr (intersperse ", " $ map show connects)
      putStrLn ""

runKmersOnFile :: FilePath -> IO T.Text
runKmersOnFile file = do
  content <- T.IO.readFile file
  let l0:l1:_ = T.lines content
  let k = (read :: String -> Int) (T.unpack l0)
  let dna = DnaString l1
  let kmersResult = kmers k dna
  return $ T.pack $ unlines $ map show kmersResult

runSpelledKmersToGenome :: FilePath -> IO T.Text
runSpelledKmersToGenome file = do
  content <- T.IO.readFile file
  let dnas = map DnaString $ T.lines content
  return $
    case spelledKmersToGenome dnas of
      Just (DnaString x) -> x
      Nothing -> T.pack ""

runOverlapGraph :: FilePath -> IO ()
runOverlapGraph file = do
  content <- T.IO.readFile file
  let dnas = map DnaString $ T.lines content
  let result = overlapGraph dnas
  printAdjacencyList result
