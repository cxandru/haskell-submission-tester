module Main where
import Test.Tasty (testGroup, Timeout(Timeout), defaultMainWithIngredients, TestTree, localOption, )
import Test.Tasty.QuickCheck as QC
import Test.Tasty.Runners.AntXML (antXMLRunner)
import GHC.Float (double2Float)
import qualified L (anteilImKreis)
import qualified S (anteilImKreis)
import Data.Function (on)

main :: IO ()
main = defaultMainWithIngredients [antXMLRunner] $ options tests

options :: TestTree -> TestTree
options = \ t -> 
    localOption (Timeout 60000000 "one minute.") $
    localOption (QuickCheckShowReplay False) $
    localOption (QuickCheckMaxSize 40) $ 
    localOption (QuickCheckTests 10000) t

tests :: TestTree
tests = testGroup "anteilImKreis"
  [ QC.testProperty "Quickcheck" $
      \(QC.Positive a) -> on (==) double2Float (S.anteilImKreis a) (L.anteilImKreis a)
  ]
