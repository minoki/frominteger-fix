import Test.Hspec
import IntegerToFloat (integerToFloat, integerToDouble, integerToBinaryFloat)
import Numeric.Floating.IEEE (fromIntegerTiesToEven) -- from fp-ieee
import GHC.Float (word2Double, word2Float, int2Double, int2Float)
import Numeric
import Test.Hspec.QuickCheck
import Test.QuickCheck

sameFloat :: RealFloat a => a -> a -> Bool
sameFloat x y | isNaN x && isNaN y = True
              | x == 0 && y == 0 = isNegativeZero x == isNegativeZero y
              | otherwise = x == y

sameFloatP :: RealFloat a => a -> a -> Property
sameFloatP x y = counterexample (showHFloat x . showString (interpret res) . showHFloat y $ "") res
  where
    res = sameFloat x y
    interpret True = " === "
    interpret False = " =/= "

prop_vs_fromInteger :: RealFloat a => (Integer -> a) -> Integer -> Property
prop_vs_fromInteger f i = f i `sameFloatP` fromInteger i

prop_vs_fromRational :: RealFloat a => (Integer -> a) -> Integer -> Property
prop_vs_fromRational f i = f i `sameFloatP` fromRational (fromInteger i)

prop_vs_fromIntegerTiesToEven :: RealFloat a => (Integer -> a) -> Integer -> Property
prop_vs_fromIntegerTiesToEven f i = f i `sameFloatP` fromIntegerTiesToEven i

prop_vs_integerToBinaryFloat :: RealFloat a => (Integer -> a) -> Integer -> Property
prop_vs_integerToBinaryFloat f i = f i `sameFloatP` integerToBinaryFloat i

genInteger :: Gen Integer
genInteger = oneof [ arbitrary -- the default
                   , chooseInteger (-2^53, 2^53)
                   , chooseInteger (-2^64, 2^64)
                   , chooseInteger (-2^128, 2^128)
                   ]

main :: IO ()
main = hspec $ modifyMaxSuccess (* 1000) $ do
  describe "integerToFloat" $ do
    -- prop "fromInteger" $ forAll genInteger $ prop_vs_fromInteger integerToFloat
    prop "fromRational" $ forAll genInteger $ prop_vs_fromRational integerToFloat
    prop "fromIntegerTiesToEven" $ forAll genInteger $ prop_vs_fromIntegerTiesToEven integerToFloat
    prop "integerToBinaryFloat" $ forAll genInteger $ prop_vs_integerToBinaryFloat integerToFloat
    prop "word2Float" $ \i -> (integerToFloat . toInteger) i `sameFloatP` word2Float i
    prop "int2Float" $ \i -> (integerToFloat . toInteger) i `sameFloatP` int2Float i
  describe "integerToDouble" $ do
    -- prop "fromInteger" $ forAll genInteger $ prop_vs_fromInteger integerToDouble
    prop "fromRational" $ forAll genInteger $ prop_vs_fromRational integerToDouble
    prop "fromIntegerTiesToEven" $ forAll genInteger $ prop_vs_fromIntegerTiesToEven integerToDouble
    prop "integerToBinaryFloat" $ forAll genInteger $ prop_vs_integerToBinaryFloat integerToDouble
    prop "word2Double" $ \i -> (integerToDouble . toInteger) i `sameFloatP` word2Double i
    prop "int2Double" $ \i -> (integerToDouble . toInteger) i `sameFloatP` int2Double i
