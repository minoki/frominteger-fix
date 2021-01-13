{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE NoImplicitPrelude #-}
module IntegerToFloat where
import           Data.Bits
import           GHC.Base        ((==), (<))
import           GHC.Exts        (Double (D#), Float (F#), Int (I#),
                                  int2Double#, int2Float#, word2Int#, (-#))
import           GHC.Float       (RealFloat (encodeFloat, floatDigits),
                                  floatDigits, negateDouble, negateFloat,
                                  roundingMode#)
import           GHC.Num         (negate, (+), (-))
import           GHC.Num.Integer (Integer (IN, IP, IS), integerLog2#,
                                  integerToInt, integerSignum#)

integerToFloat :: Integer -> Float
integerToFloat (IS i)   = F# (int2Float# i)
integerToFloat i@(IP _) = integerToBinaryFloat' i
integerToFloat (IN bn)  = negateFloat (integerToBinaryFloat' (IP bn))
{-# NOINLINE integerToFloat #-}

integerToDouble :: Integer -> Double
integerToDouble (IS i)   = D# (int2Double# i)
integerToDouble i@(IP _) = integerToBinaryFloat' i
integerToDouble (IN bn)  = negateDouble (integerToBinaryFloat' (IP bn))
{-# NOINLINE integerToDouble #-}

-- Suitable definition for fromInteger
integerToBinaryFloat :: RealFloat a => Integer -> a
integerToBinaryFloat i = case integerSignum# i of
                           0# -> encodeFloat 0 0
                           1# -> integerToBinaryFloat' i
                           _  {- -1# -} -> negate (integerToBinaryFloat' (negate i))

-- Invariant: n > 0
integerToBinaryFloat' :: RealFloat a
                      => Integer -- ^ Must be @> 0@.
                      -> a
integerToBinaryFloat' n = result
  where
    -- @floatRadix result@ must be 2
    mantDigs = floatDigits result
    k = I# (word2Int# (integerLog2# n))
    result = if k < mantDigs then
               encodeFloat n 0
             else
               let e@(I# e#) = k - mantDigs + 1
                   q = n `unsafeShiftR` e
                   n' = case roundingMode# n (e# -# 1#) of
                          0# -> q
                          1# -> if integerToInt q .&. 1 == 0 then
                                  q
                                else
                                  q + 1
                          _ {- 2# -} -> q + 1
               in encodeFloat n' e
{-# SPECIALIZE integerToBinaryFloat :: Integer -> Float, Integer -> Double #-}
