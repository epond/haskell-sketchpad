module HaskellProgramming.Chapter10Spec where

import Test.Hspec
import HaskellProgramming.Chapter10
import Data.Time

spec :: Spec
spec = do
    describe "Database Processing" $ do
        it "has a function that filters for DbDate and returns a list of UTCTime values inside them" $ do
            filterDbDate theDatabase `shouldBe` [UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123), UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123)]
        it "has a function that filters for DbNumber and returns a list of the Integer values inside them" $ do
            filterDbNumber theDatabase `shouldBe` [9001]