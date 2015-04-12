{-# LANGUAGE OverloadedStrings #-}

module ApplicativesSpec where

import Test.Hspec
import Applicatives
import Control.Applicative

success3 :: Validation String Int
success3 = Success 3

spec :: Spec
spec = do
    describe "My Validation type" $ do
      it "can be used as a functor" $ do
        fmap (+1) success3 `shouldBe` Success 4
        fmap (+1) (Failure ["no"] :: Validation String Int) `shouldBe` Failure ["no"]
      it "obeys the functor laws" $ do
        -- TODO use quickcheck
        -- identity law
        fmap id success3 `shouldBe` Success 3
        fmap id (Failure ["no"] :: Validation String Int) `shouldBe` Failure ["no"]
        -- associativity law
        fmap ((+1).(*2)) success3 `shouldBe` fmap (+1) (fmap (*2) success3)
      it "can be used as an applicative" $ do
        (+3) <$> success3 `shouldBe` Success 6
        (*) <$> success3 <*> success3 `shouldBe` Success 9
        (*) <$> (Failure ["a"] :: Validation String Int) <*> Failure ["b"] `shouldBe` Failure["a", "b"]
      it "obeys the applicative laws" $ do
        -- TODO use quickcheck
        Success (*2) <*> success3 `shouldBe` fmap (*2) success3
        -- TODO identity law: pure id <*> v = v
        -- TODO composition law: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
        -- TODO homomorphism law: pure f <*> pure x = pure (f x)
        -- TODO interchange law: u <*> pure y = pure ($ y) <*> u
