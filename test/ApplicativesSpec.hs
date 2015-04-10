{-# LANGUAGE OverloadedStrings #-}

module ApplicativesSpec where

import Test.Hspec
import Applicatives

spec :: Spec
spec = do
    describe "My Validation type" $ do
      it "obeys the functor laws" $ do
        pending
      it "obeys the applicative laws" $ do
        pending
