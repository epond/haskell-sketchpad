module HaskellProgramming.Chapter12AdversitySpec where

import Test.Hspec
import HaskellProgramming.Chapter12Adversity

spec :: Spec
spec = do
    describe "String processing" $ do
        it "can replace 'the' with 'a'" $ do
            replaceThe "the cow loves us" `shouldBe` "a cow loves us"
        it "can count the number of instances of 'the' followed by a vowel-initial word" $ do
            countTheBeforeVowel "the cow" `shouldBe` 0
            countTheBeforeVowel "the evil cow" `shouldBe` 1
            countTheBeforeVowel "the evil cow mooed beneath the sun in the afternoon" `shouldBe` 2