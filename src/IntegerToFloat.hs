{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE NoImplicitPrelude #-}
module IntegerToFloat where
import           Data.Bits
import           GHC.Base        ((<), (==))
import           GHC.Exts        (Double (D#), Double#, Float (F#), Float#,
                                  Int (I#), int2Double#, int2Float#,
                                  negateDouble#, negateFloat#, word2Int#, (-#))
import           GHC.Float       (RealFloat (encodeFloat, floatDigits),
                                  floatDigits, negateDouble, negateFloat,
                                  roundingMode#)
import           GHC.Num         (negate, (+), (-))
import           GHC.Num.Integer (Integer (IN, IP, IS), integerLog2#,
                                  integerSignum#, integerToInt)

integerToFloat :: Integer -> Float
integerToFloat i = F# (integerToFloat# i)

integerToFloat# :: Integer -> Float#
integerToFloat# (IS i)   = int2Float# i
integerToFloat# i@(IP _) = case integerToBinaryFloat' i of
                             F# x -> x
integerToFloat# (IN bn)  = case integerToBinaryFloat' (IP bn) of
                             F# x -> negateFloat# x
{-# NOINLINE integerToFloat# #-}

integerToDouble :: Integer -> Double
integerToDouble i = D# (integerToDouble# i)

integerToDouble# :: Integer -> Double#
integerToDouble# (IS i)   = int2Double# i
integerToDouble# i@(IP _) = case integerToBinaryFloat' i of
                              D# x -> x
integerToDouble# (IN bn)  = case integerToBinaryFloat' (IP bn) of
                              D# x -> negateDouble# x
{-# NOINLINE integerToDouble# #-}

-- Suitable definition for fromInteger
integerToBinaryFloat :: RealFloat a => Integer -> a
integerToBinaryFloat i = case integerSignum# i of
                           0# -> encodeFloat 0 0
                           1# -> integerToBinaryFloat' i
                           _  {- -1# -} -> negate (integerToBinaryFloat' (negate i))

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
