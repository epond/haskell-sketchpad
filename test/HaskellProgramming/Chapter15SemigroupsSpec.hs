module HaskellProgramming.Chapter15SemigroupsSpec where

import Test.Hspec
import Test.QuickCheck
import HaskellProgramming.Chapter15Semigroups
import Data.Semigroup

spec :: Spec
spec = do
    describe "The Semigroup instance for Trivial" $ do
        it "satisfies the associativity law" $ do
            property (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
    describe "The Semigroup instance for Identity" $ do
        it "satisfies the associativity law on Trivial" $ do
            property (semigroupAssoc :: Identity Trivial -> Identity Trivial -> Identity Trivial -> Bool)
        it "satisfies the associativity law on String" $ do
            property (semigroupAssoc :: Identity String -> Identity String -> Identity String -> Bool)
    describe "The Semigroup instance for Two" $ do
        it "satisfies the associativity law" $ do
            property (semigroupAssoc :: Two String Bool -> Two String Bool -> Two String Bool -> Bool)
    describe "The Semigroup instance for Three" $ do
        it "satisfies the associativity law" $ do
            property (semigroupAssoc :: Three String String String -> Three String String String -> Three String String String -> Bool)
    describe "The Semigroup instance for Four" $ do
        it "satisfies the associativity law" $ do
            property (semigroupAssoc :: Four String String String String -> Four String String String String -> Four String String String String -> Bool)
        
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

instance Arbitrary Trivial where
    arbitrary = return Trivial

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return (Identity x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Two x y)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Three x y z)
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        z' <- arbitrary
        return (Four x y z z')        