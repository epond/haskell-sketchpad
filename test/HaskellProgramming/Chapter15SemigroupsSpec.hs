module HaskellProgramming.Chapter15SemigroupsSpec where

import Test.Hspec
import Test.QuickCheck
import HaskellProgramming.Chapter15Semigroups
import Data.Semigroup

instance Arbitrary Trivial where
    arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

spec :: Spec
spec = do
    describe "The Semigroup instance for Trivial" $ do
        it "satisfies the associativity law" $ do
            property (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)