module QuickCheckSpec where

import Test.Hspec
import Test.QuickCheck

-- see http://hspec.github.io/quickcheck.html
-- see https://hackage.haskell.org/package/QuickCheck-2.8.1/docs/Test-QuickCheck.html
spec :: Spec
spec = do
  describe "read" $ do
    context "when used with ints" $ do
        it "is inverse to show" $ property $
            \x -> (read . show) x == (x :: Int)
  describe "add" $ do
    context "when used with Integer" $ do
        it "is commutative" $ property prop_commutativeAdd
  describe "a custom Point data type" $ do
    it "swapPoint . swapPoint = id" $ property prop_swapInvolution


prop_commutativeAdd :: Integer -> Integer -> Bool
prop_commutativeAdd n m = n + m == m + n

data Point = MkPoint Int Int deriving (Show, Eq)

instance Arbitrary Point where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (MkPoint x y)

swapPoint :: Point -> Point
swapPoint (MkPoint x y) = MkPoint y x

prop_swapInvolution :: Point -> Bool
prop_swapInvolution point = swapPoint (swapPoint point) == point