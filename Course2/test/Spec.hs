import Test.Hspec
import Lib


main :: IO ()
main = hspec $ do
  describe "Kmers in a DnaString" $ do
    it "returns expected values for sample" $ do
      DnaString.kmers 5 dnaString `shouldBe` kmers
      where
        dnaString = ""
        kmers = [""]

