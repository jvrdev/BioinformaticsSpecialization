import Test.Hspec
import qualified Data.Text as T
import Lib

main :: IO ()
main = hspec $ do
  describe "Kmers in a DnaString" $ do
    it "returns expected values for sample" $ do
      kmers k dnaString `shouldBe` expectedKmers
      where
        k = 5
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

