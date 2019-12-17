module Lib
    ( DnaString(..)
    , slice
    , kmers
    , spelledKmersToGenome
    , overlapGraph
    , deBruijnGraph
    , deBruijnGraphFromKmers
    , eulerianCycle
    , runKmersOnFile
    , runSpelledKmersToGenome
    , runOverlapGraph
    , runDeBruijnGraph
    , runDeBruijnGraphFromKmers
    , AdjacencyListEntry(..)
    , AdjacencyList(..)
    ) where

import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Data.HashMap.Strict.HashMap as HashMap

data DnaString = DnaString T.Text deriving (Eq, Ord)

instance Show DnaString where
  show (DnaString s) = T.unpack s

data AdjacencyListEntry a = ALE (a, [a]) deriving (Eq, Show, Ord)

aleNode :: AdjacencyListEntry a -> a
aleNode (ALE (n, _)) = n

aleEdges :: AdjacencyListEntry a -> [a]
aleEdges (ALE (_, es)) = es

aleOrder :: Ord a => AdjacencyListEntry a -> AdjacencyListEntry a -> Ordering
aleOrder a b = compare (aleNode a) (aleNode b)
         
instance Functor AdjacencyListEntry where
  fmap f (ALE (node, edges)) = ALE (f node, map f edges)
  
data AdjacencyList a = AL [AdjacencyListEntry a] deriving (Eq, Show, Ord)

instance Functor AdjacencyList where
  fmap f (AL entries) = AL $ map (fmap f) entries

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
  AL $ filter sndIsNotEmpty $ map adjacencyEntry $ xs
  where
    adjacencyEntry a = ALE (a, filter (connectsWith a) xs)
    sndIsNotEmpty (ALE (_, edges)) = not $ null $ edges 

deBruijnGraphFromKmers :: [DnaString] -> AdjacencyList DnaString
deBruijnGraphFromKmers edges =
  AL $ map mergeAle $ groupBy sameNode $ sort $ map edgeToAle edges 
  where
    edgeToAle (DnaString edge) =
      ALE $
      (DnaString $ T.dropEnd 1 edge
      , [DnaString $ T.drop 1 edge])
    sameNode (ALE (a, _)) (ALE (b, _)) = a == b
    mergeAle ales = ALE (aleNode $ head ales, sort $ concat $ map aleEdges ales)

deBruijnGraph :: Int -> DnaString -> AdjacencyList DnaString
deBruijnGraph k dna = deBruijnGraphFromKmers $ kmers k dna

rotate :: Int [a] -> [a]
rotate n xs = bs ++ as where (as, bs) = splitAt n xs

consumeEdge :: a -> HashMap a [a] -> (HashMap a [a], Maybe a)
consumeEdge node edges =
  case HashMap.lookup node edges of
    Nothing -> (edges, Nothing)
    Just [x] -> (HashMap.delete node edges, Just x)
    Just (x:y) -> (HashMap.insert node y, Just x)

walkCycle :: HashMap a [a] -> [a] -> (HashMap a [a], [a])
walkCycle available walk =
  loop (available, startCycle)
  where
    loop :: (HashMap a [a], [a]) -> (HashMap a [a], [a])
    loop (avs, (node:path)) =
      let (next, avs') = consumeEdge node avs in
        path' = case next of
          Just x -> x:path
          Nothing -> path
    startCycle =
      if null walk
      then [firstNodeAvailable]
      else startCycle
      where
        firstNodeAvailable = head $ HashMap.keys available
        firstNodeInCycleAvailable = first (\node -> HashMap.member node available) walk
        startCycle = rotate (fromJust $ elemIndex startNode walk) walk

eulerianCycle :: AdjacencyList a -> [a]
eulerianCycle (AL entries) = [x]
  loop available []
  where
    available = HashMap.fromList $ map (\(ALE x) -> x) entries
    loop available walk =
      if HashMap.null available
      then walk
      else
        let (available', walk') = walkCycle available walk
        in loop available' walk'

printAdjacencyList :: Show a => AdjacencyList a -> IO ()
printAdjacencyList (AL xs) =
  mapM_ f xs
  where
    f (ALE (node, connects)) = do
      putStr $ show node
      putStr " -> "
      mapM_ putStr (intersperse "," $ map show connects)
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

runDeBruijnGraph :: FilePath -> IO ()
runDeBruijnGraph file = do
  content <- T.IO.readFile file
  let l0:l1:_ = T.lines content
  let k = (read :: String -> Int) (T.unpack l0)
  let dnas = DnaString l1
  let result = deBruijnGraph k dnas
  printAdjacencyList result

runDeBruijnGraphFromKmers :: FilePath -> IO ()
runDeBruijnGraphFromKmers file = do
  content <- T.IO.readFile file
  let dnas = map DnaString $ T.lines content
  let result = deBruijnGraphFromKmers dnas
  printAdjacencyList result
