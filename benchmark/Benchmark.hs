{-# LANGUAGE NumericUnderscores #-}
import Numeric.Floating.IEEE (fromIntegerTiesToEven)
import Gauge.Main
import IntegerToFloat (integerToFloat, integerToDouble, integerToBinaryFloat)

main :: IO ()
main = defaultMain
  [ bgroup "Float"
    [ bgroup label $ map ($ arg)
      [ bench "fromInteger" . whnf (fromInteger :: Integer -> Float)
      , bench "fromRational" . whnf (fromRational . fromInteger :: Integer -> Float)
      , bench "integerToFloat" . whnf integerToFloat
      , bench "fromIntegerTiesToEven (fp-ieee)" . whnf (fromIntegerTiesToEven :: Integer -> Float)
      , bench "integerToBinaryFloat" . whnf (integerToBinaryFloat :: Integer -> Float)
      ]
    | (label,arg) <- [("small", 42)
                     ,("negative small", -42)
                     ,("large", 2^128 - 1)
                     ,("large", 2^128 - 1)
                     ,("negative large", -(2^128-1))
                     ]
    ]
  , bgroup "Double"
    [ bgroup label $ map ($ arg)
      [ bench "fromInteger" . whnf (fromInteger :: Integer -> Double)
      , bench "fromRational" . whnf (fromRational . fromInteger :: Integer -> Double)
      , bench "integerToDouble" . whnf integerToDouble
      , bench "fromIntegerTiesToEven (fp-ieee)" . whnf (fromIntegerTiesToEven :: Integer -> Double)
      , bench "integerToBinaryFloat" . whnf (integerToBinaryFloat :: Integer -> Double)
      ]
    | (label,arg) <- [("small", 42)
                     ,("negative small", -42)
                     ,("large", 2^128 - 1)
                     ,("negative large", -(2^128 - 1))
                     ]
    ]
  ]
