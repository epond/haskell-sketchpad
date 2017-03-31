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
        it "has a function that returns the most recent time" $ do
            mostRecent theDatabase `shouldBe` UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123)
        it "has a function that sums up all the DbNumbers" $ do
            let db1 = []
            sumDb db1 `shouldBe` 0
            let db2 = [DbNumber 24, DbString "5", DbNumber 76]
            sumDb db2 `shouldBe` 100