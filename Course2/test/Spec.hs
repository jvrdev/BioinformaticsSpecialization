import Data.List
import qualified Data.Text as T
import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Lib" $ do
    describe "Kmers in a DnaString" $ do
      it "returns expected values for sample" $ do
        let k = 5
            dnaString = DnaString $ T.pack "ACGTACGTACGT"
            expectedKmers = map
                    (DnaString . T.pack)
                    [ "ACGTA"
                    , "CGTAC"
                    , "GTACG"
                    , "TACGT"
                    , "ACGTA"
                    , "CGTAC"
                    , "GTACG"
                    , "TACGT"]
        kmers k dnaString `shouldBe` expectedKmers

    describe "Spelled kmers to genome" $ do
      it "returns expected values for sample" $ do
        let dnaString = Just $ DnaString $ T.pack "ACCGAAGCT"
            spelledKmers = map
                    (DnaString . T.pack)
                    [ "ACCGA"
                    , "CCGAA"
                    , "CGAAG"
                    , "GAAGC"
                    , "AAGCT"]
        spelledKmersToGenome spelledKmers `shouldBe` dnaString

    describe "Overlap graph" $ do
      it "returns expected values for sample" $ do
        let f = DnaString . T.pack
        let input =
                map
                f
                $
                sort
                [ "ATGCG"
                , "GCATG"
                , "CATGC"
                , "AGGCA"
                , "GGCAT"
                , "GGCAC" ]
        let expectedOutput =
                fmap f
                $ AL                
                [ ALE ("AGGCA", ["GGCAC", "GGCAT"])
                , ALE ("CATGC", ["ATGCG"])
                , ALE ("GCATG", ["CATGC"])
                , ALE ("GGCAT", ["GCATG"]) ] 
        overlapGraph input `shouldBe` expectedOutput

    describe "Solve the De Bruijn Graph from a String Problem" $ do
      it "returns expected values for sample" $ do
        let f = DnaString . T.pack
        let input = f "AAGATTCTCTAAGA"
        let expectedOutput = fmap f $ AL $ map ALE
                [ ("AAG",  ["AGA", "AGA"])
                , ("AGA",  ["GAT"])
                , ("ATT",  ["TTC"])
                , ("CTA",  ["TAA"])
                , ("CTC",  ["TCT"])
                , ("GAT",  ["ATT"])
                , ("TAA",  ["AAG"])
                , ("TCT",  ["CTA", "CTC"])
                , ("TTC",  ["TCT"]) ]
        deBruijnGraph 4 input `shouldBe` expectedOutput
    describe "Solve the DeBruijn Graph from k-mers Problem" $ do
      it "returns expected values for sample" $ do
        let f = DnaString . T.pack
        let input =
                map f [
                     "GAGG"
                   , "CAGG"
                   , "GGGG"
                   , "GGGA"
                   , "CAGG"
                   , "AGGG"
                   , "GGAG"
                   ]
        let expectedOutput = fmap f $ AL $ map ALE
                [ ("AGG",  ["GGG"])
                , ("CAG",  ["AGG", "AGG"])
                , ("GAG",  ["AGG"])
                , ("GGA",  ["GAG"])
                , ("GGG",  ["GGA", "GGG"]) ]
        deBruijnGraphFromKmers input `shouldBe` expectedOutput
                     

