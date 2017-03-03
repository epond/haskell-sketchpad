module HaskellProgramming.Chapter09Spec where

import Test.Hspec
import HaskellProgramming.Chapter09

spec :: Spec
spec = do
    describe "Chapter09" $ do
        it "eftBool True False gives empty list" $ do
            eftBool True False `shouldBe` []
        it "eftBool False True gives [False, True]" $ do
            eftBool False True `shouldBe` [False, True]
        it "eftBool False False gives [False]" $ do
            eftBool False False `shouldBe` [False]
        it "eftBool True True gives [True]" $ do
            eftBool True True `shouldBe` [True]