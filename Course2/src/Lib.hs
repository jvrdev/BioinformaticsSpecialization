module Lib
    ( someFunc
    , DnaString
    , kmers
    ) where

import qualified Data.Text as T
import Data.String

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data DnaString = DnaString T.Text

kmers :: DnaString -> [DnaStr

kmers (DnaString x) = [DnaString x]
