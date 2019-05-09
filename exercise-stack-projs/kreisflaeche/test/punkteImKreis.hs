module Main where
import Test.Tasty (testGroup, Timeout(Timeout), defaultMainWithIngredients, TestTree, localOption, )
import Test.Tasty.QuickCheck as QC
import Test.Tasty.Runners.AntXML (antXMLRunner)
import qualified L (punkteImKreis)
import qualified S (punkteImKreis)
import Data.Set (fromList)
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
tests = testGroup "punkteImKreis"
  [ QC.testProperty "(Q) general check" $
      \(QC.Positive a) -> on (==) fromList (S.punkteImKreis a) (L.punkteImKreis a)
  ]
