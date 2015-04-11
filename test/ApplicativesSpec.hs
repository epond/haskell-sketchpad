{-# LANGUAGE OverloadedStrings #-}

module ApplicativesSpec where

import Test.Hspec
import Applicatives
import Control.Applicative

spec :: Spec
spec = do
    describe "My Validation type" $ do
      it "can be used as a functor" $ do
        fmap (+1) (Success 3 :: Validation String Int) `shouldBe` Success 4
        fmap (+1) (Failure ["no"] :: Validation String Int) `shouldBe` Failure ["no"]
      it "obeys the functor laws" $ do
        -- TODO use quickcheck
        -- identity law
        fmap id (Success 3 :: Validation String Int) `shouldBe` Success 3
        fmap id (Failure ["no"] :: Validation String Int) `shouldBe` Failure ["no"]
        -- associativity law
        let initialVal = (Success 3 :: Validation String Int)
        fmap ((+1).(*2)) initialVal `shouldBe` fmap (+1) (fmap (*2) initialVal)
      it "can be used as an applicative" $ do
        (+3) <$> (Success 1 :: Validation String Int) `shouldBe` Success 4
        (*) <$> (Success 2 :: Validation String Int) <*> Success 3 `shouldBe` Success 6
        (*) <$> (Failure ["a"] :: Validation String Int) <*> Failure ["b"] `shouldBe` Failure["a", "b"]
      it "obeys the applicative laws" $ do
        -- TODO use quickcheck
        let initialVal = (Success 3 :: Validation String Int)
        Success (*2) <*> initialVal `shouldBe` fmap (*2) initialVal
        -- TODO identity law: pure id <*> v = v
        -- TODO composition law: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
        -- TODO homomorphism law: pure f <*> pure x = pure (f x)
        -- TODO interchange law: u <*> pure y = pure ($ y) <*> u
