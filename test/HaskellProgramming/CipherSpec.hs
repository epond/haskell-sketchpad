module HaskellProgramming.CipherSpec where

import Test.Hspec
import HaskellProgramming.Cipher

spec :: Spec
spec = do
    describe "Cipher" $ do
        it "encodes" $ do
            caesar 3 "abc" `shouldBe` "def"
        it "decodes" $ do
            unCaesar 3 "def" `shouldBe` "abc"