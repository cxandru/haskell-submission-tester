module Main where
import Test.Tasty (testGroup, defaultMainWithIngredients, TestTree, localOption, )
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU
import Test.Tasty.Runners.AntXML (antXMLRunner)
import qualified L (foo)
import S (foo)
import Data.Function (on)

main :: IO ()
main = defaultMainWithIngredients [antXMLRunner] $ options tests

options :: TestTree -> TestTree
options = \ t -> 
    localOption (QuickCheckShowReplay False) $
    localOption (QuickCheckMaxSize 40) $ 
    localOption (QuickCheckTests 1000) t

tests :: TestTree
tests = testGroup "{S}: foo"
  [ QC.testProperty "Quickcheck" $
      \a -> on (===) ($ a) S.foo L.foo
  , HU.testCase "HUnit" $
    S.foo undefined @?= undefined
  ]
