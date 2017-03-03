module HaskellProgramming.Chapter09Spec where

import Test.Hspec
import HaskellProgramming.Chapter09

spec :: Spec
spec = do
    describe "Chapter09 eftBool" $ do
        it "eftBool True False gives empty list" $ do
            eftBool True False `shouldBe` []
        it "eftBool False True gives [False, True]" $ do
            eftBool False True `shouldBe` [False, True]
        it "eftBool False False gives [False]" $ do
            eftBool False False `shouldBe` [False]
        it "eftBool True True gives [True]" $ do
            eftBool True True `shouldBe` [True]

    describe "Chapter09 eftOrd" $ do
        it "eftOrd LT GT" $ do
            eftOrd LT GT `shouldBe` [LT, EQ, GT]
        it "eftOrd EQ EQ" $ do
            eftOrd EQ EQ `shouldBe` [EQ]
        it "eftOrd GT LT" $ do
            eftOrd GT LT `shouldBe` []
        it "eftOrd LT EQ" $ do
            eftOrd LT EQ `shouldBe` [LT, EQ]
        it "eftOrd EQ GT" $ do
            eftOrd EQ GT `shouldBe` [EQ, GT]

    describe "Chapter09 eftInt" $ do
        it "eftInt 3 9" $ do
            eftInt 3 9 `shouldBe` [3, 4, 5, 6, 7, 8, 9]
        it "eftInt 9 3" $ do
            eftInt 9 3 `shouldBe` []
        it "eftInt 9 9" $ do
            eftInt 9 9 `shouldBe` [9]

    describe "Chapter09 eftChar" $ do
        it "eftChar 'c' 'f'" $ do
            eftChar 'c' 'f' `shouldBe` ['c', 'd', 'e', 'f']
        it "eftChar 'f' 'c'" $ do
            eftChar 'f' 'c' `shouldBe` []
        it "eftChar 'c' 'c'" $ do
            eftChar 'c' 'c' `shouldBe` ['c']
