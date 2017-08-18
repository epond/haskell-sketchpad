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
        it "can count the number of letters in a word that are a vowel" $ do
            countVowels "the cow" `shouldBe` 2
            countVowels "rhythm" `shouldBe` 0
            countVowels "anenome" `shouldBe` 4
    describe "Validate the word" $ do
        it "can validate a word based upon number of vowels versus consonants" $ do
            mkWord "doolalay" `shouldBe` Just (Word' "doolalay")
            mkWord "doolaalay" `shouldBe` Nothing
    describe "It's only Natural" $ do
        it "can convert from Nat to Integer" $ do
            natToInteger Zero `shouldBe` 0
            natToInteger (Succ Zero) `shouldBe` 1
            natToInteger (Succ (Succ (Succ Zero))) `shouldBe` 3