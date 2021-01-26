{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

#include "MachDeps.h"

module IntegerToFloat where
import           Data.Bits
import           GHC.Base           ((<), (==))
import           GHC.Exts
import           GHC.Float          (RealFloat (encodeFloat, floatDigits),
                                     floatDigits, negateDouble, negateFloat,
                                     roundingMode#)
import           GHC.Num            (negate, (+), (-))
import           GHC.Num.Integer    (Integer (IN, IP, IS), integerLog2#,
                                     integerSignum#, integerToInt)
import           GHC.Num.Primitives (intEncodeDouble#, wordLog2#)

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
integerToBinaryFloat (IS 0#) = encodeFloat 0 0
integerToBinaryFloat i@(IS i#) = if isTrue# (i# ># 0#) then
                                   integerToBinaryFloat' i
                                 else
                                   negate (integerToBinaryFloat' (IS (negateInt# i#)))
integerToBinaryFloat i@(IP _) = integerToBinaryFloat' i
integerToBinaryFloat (IN bn) = negate (integerToBinaryFloat' (IP bn))
{-# SPECIALIZE integerToBinaryFloat :: Integer -> Float, Integer -> Double #-}

integerToBinaryFloatX :: RealFloat a => Integer -> a
integerToBinaryFloatX (IS 0#) = encodeFloat 0 0
integerToBinaryFloatX (IS i) = if isTrue# (i ># 0#) then
                                 intToBinaryFloat' i
                               else
                                 negate (intToBinaryFloat' (negateInt# i))
integerToBinaryFloatX i@(IP _) = integerToBinaryFloat' i
integerToBinaryFloatX (IN bn) = negate (integerToBinaryFloat' (IP bn))
{-# SPECIALIZE integerToBinaryFloatX :: Integer -> Float, Integer -> Double #-}

{-
integerToBinaryFloatS :: RealFloat a => Integer -> a
integerToBinaryFloatS i = case integerSignum# i of
                           0# -> encodeFloat 0 0
                           1# -> integerToBinaryFloat' i
                           _  {- -1# -} -> negate (integerToBinaryFloat' (negate i))
-}

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
               let !e@(I# e#) = k - mantDigs + 1
                   q = n `unsafeShiftR` e
                   n' = case roundingMode# n (e# -# 1#) of
                          0# -> q
                          1# -> if integerToInt q .&. 1 == 0 then
                                  q
                                else
                                  q + 1
                          _ {- 2# -} -> q + 1
               in encodeFloat n' e
{-# SPECIALIZE integerToBinaryFloat' :: Integer -> Float, Integer -> Double #-}

intToBinaryFloat' :: RealFloat a
                  => Int# -- ^ Must be @> 0@.
                  -> a
intToBinaryFloat' n = result
  where
    -- @floatRadix result@ must be 2
    mantDigs = floatDigits result
    k = I# (word2Int# (wordLog2# (int2Word# n)))
    result = if k < mantDigs then
               encodeFloat (IS n) 0
             else
               let !e@(I# e#) = k - mantDigs + 1
                   !q = n `uncheckedIShiftRL#` e#
                   !n' = case roundingMode# (IS n) (e# -# 1#) of -- ?
                           0# -> q
                           1# -> if isTrue# (q `andI#` 1# ==# 0#) then
                                   q
                                 else
                                   q +# 1#
                           _ {- 2# -} -> q +# 1#
               in encodeFloat (IS n') e
{-# SPECIALIZE intToBinaryFloat' :: Int# -> Float, Int# -> Double #-}

--
-- Specialized implementations
--

integerToFloatS :: Integer -> Float
integerToFloatS i = F# (integerToFloatS# i)

integerToFloatS# :: Integer -> Float#
integerToFloatS# (IS i)   = int2Float# i
integerToFloatS# i@(IP _) = integerToFloat' i
integerToFloatS# (IN bn)  = negateFloat# (integerToFloat' (IP bn))
{-# NOINLINE integerToFloatS# #-}

integerToFloat' :: Integer -- ^ Must be @>= 2^(WORD_SIZE_IN_BITS-1)@.
                -> Float#
integerToFloat' n = result
  where
    -- @floatRadix result@ must be 2
    -- FLT_MANT_DIG must be 23
    mantDigs = floatDigits (0 :: Float)
    k = I# (word2Int# (integerLog2# n))
    -- k >= mantDigs
    result = let !e@(I# e#) = k - mantDigs + 1
                 !q@(I# q#) = integerToInt (n `unsafeShiftR` e)
                 !n' = case roundingMode# n (e# -# 1#) of
                         0# -> q#
                         1# -> if q .&. 1 == 0 then
                                 q#
                               else
                                 q# +# 1#
                         _ {- 2# -} -> q# +# 1#
             in double2Float# (intEncodeDouble# n' e#)

integerToDoubleS :: Integer -> Double
integerToDoubleS i = D# (integerToDoubleS# i)

integerToDoubleS# :: Integer -> Double#
integerToDoubleS# (IS i)   = int2Double# i
integerToDoubleS# i@(IP _) = integerToDouble' i
integerToDoubleS# (IN bn)  = negateDouble# (integerToDouble' (IP bn))
{-# NOINLINE integerToDoubleS# #-}

integerToDouble' :: Integer -- ^ Must be @>= 2^(WORD_SIZE_IN_BITS-1)@.
                 -> Double#
integerToDouble' n = result
  where
    -- @floatRadix result@ must be 2
    mantDigs = floatDigits (0 :: Double)
    k = I# (word2Int# (integerLog2# n))
#if DBL_MANT_DIGS > WORD_SIZE_IN_BITS
#error "Unsupported configuration"
#else
    result = let !e@(I# e#) = k - mantDigs + 1
                 !q@(I# q#) = integerToInt (n `unsafeShiftR` e)
                 !n' = case roundingMode# n (e# -# 1#) of
                         0# -> q#
                         1# -> if q .&. 1 == 0 then
                                 q#
                               else
                                 q# +# 1#
                         _ {- 2# -} -> q# +# 1#
             in intEncodeDouble# n' e#
#endif
