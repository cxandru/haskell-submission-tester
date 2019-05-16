

 {-# OPTIONS_GHC -Wincomplete-patterns #-}

{-# LANGUAGE Template-Haskell,
 OverloadedStrings #-}


sort2 :: (Int, Int) -> (Int, Int)
sort2 (y,x)
  | y < x = (y,x)
  | x < y = (x,y)
  | otherwise = (y,x)
