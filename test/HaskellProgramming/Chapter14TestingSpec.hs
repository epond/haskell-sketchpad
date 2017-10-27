module HaskellProgramming.Chapter14TestingSpec where

import Test.Hspec
import Test.QuickCheck
import HaskellProgramming.Chapter14Testing
import Data.List (sort)

spec :: Spec
spec = do
    describe "Example-based test: My dividedBy function" $ do
        it "15 divided by 3 is 5" $ do
            dividedBy 15 3 `shouldBe` (5, 0)
        it "22 divided by 5 is 4 remainder 2" $ do
            dividedBy 22 5 `shouldBe` (4, 2)
    describe "Chapter Exercises: Using QuickCheck" $ do
        it "inline property: x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)
        it "can check the half function" $ do
            property prop_halfIdentity
        it "can check that a sorted list Int is ordered" $ do
            property $ forAll (arbitrary :: Gen [Int]) (\x -> listOrdered (sort x))

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
           | n < d = (count, n)
           | otherwise = go (n - d) d (count + 1)

half :: Fractional a => a -> a
half x = x / 2

prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = (((*2) . half) x) == x

-- another way of saying the same thing but it shows where we could introduce a
-- generator for the Double type if we were not happy with the default
prop_halfIdentity' :: Property
prop_halfIdentity' =
    forAll (arbitrary :: Gen Double) (\x -> (((*2) . half) x) == x)

-- for any list you apply sort to this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t) = (Just y, t)
          go y (Just x, t) = (Just y, x >= y)