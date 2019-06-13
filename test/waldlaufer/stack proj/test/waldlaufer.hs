module Main where

import Test.Tasty (defaultMain)
import Test.Tasty.HUnit as HU

main :: IO ()
main = defaultMain $ HU.testCase "waldlaufer" $ f 0 @?= 1


f x = f $ x+1
