module S (punkteImKreis, anteilImKreis)
where

punkteImKreis :: Double -> [( Double , Double ) ]
punkteImKreis k =
  [( x , y ) | x <- [1.. k ] , y <- [1.. k ] , sqrt ( x ^2 + y ^2) < k ]
anteilImKreis :: Double -> Double
anteilImKreis k =
  ( fromIntegral ( length ( punkteImKreis k ) ) ) / ( k * k )
