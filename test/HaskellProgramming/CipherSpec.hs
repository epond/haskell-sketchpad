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
        it "can rotate forward by character" $ do
            rotForwardByChar 'a' 'a' `shouldBe` 'a'
            rotForwardByChar 'c' 'a' `shouldBe` 'c'
            rotForwardByChar 'c' 'd' `shouldBe` 'f'
            rotForwardByChar 'd' 'y' `shouldBe` 'b'
        it "can rotate backward by character" $ do
            rotBackwardByChar 'a' 'a' `shouldBe` 'a'
            rotBackwardByChar 'c' 'c' `shouldBe` 'a'
            rotBackwardByChar 'c' 'f' `shouldBe` 'd'
            rotBackwardByChar 'd' 'b' `shouldBe` 'y'
        it "encodes" $ do
            vigenere "ally" "meetatdawn" `shouldBe` "mppraeoywy"
        it "decodes" $ do
            unVigenere "ally" "mppraeoywy" `shouldBe` "meetatdawn"