module HaskellProgramming.CipherSpec where

import Test.Hspec
import HaskellProgramming.Cipher

spec :: Spec
spec = do
    describe "Caesar Cipher" $ do
        it "encodes" $ do
            caesar 3 "abc" `shouldBe` "def"
        it "decodes" $ do
            unCaesar 3 "def" `shouldBe` "abc"
    describe "Vigenere Cipher" $ do
        it "encodes" $ do
            vigenere "ally" "meetatdawn" `shouldBe` "mppraeoywy"
        it "decodes" $ do
            unVigenere "ally" "mppraeoywy" `shouldBe` "meetatdawn"