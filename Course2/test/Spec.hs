import Test.Hspec
import qualified Data.Text as T
import Lib

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


