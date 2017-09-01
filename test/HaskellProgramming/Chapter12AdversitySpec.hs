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
        it "can convert from Integer to Maybe Nat" $ do
            integerToNat 0 `shouldBe` (Just Zero)
            integerToNat 1 `shouldBe` (Just (Succ Zero))
            integerToNat 2 `shouldBe` (Just (Succ (Succ Zero)))
            integerToNat (-1) `shouldBe` Nothing
    describe "Small library for Maybe" $ do
        it "can determine if a Maybe has a value" $ do
            isJust Nothing `shouldBe` False
            isJust (Just 1) `shouldBe` True
        it "can determine is a Maybe does not have a value" $ do
            isNothing Nothing `shouldBe` True
            isNothing (Just 1) `shouldBe` False
        it "has a catamorphism for Maybe" $ do
            mayybee 0 (+1) Nothing `shouldBe` 0
            mayybee 0 (+1) (Just 1) `shouldBe` 2
        it "can provide a fallback value" $ do
            fromMaybe 0 (Just 5) `shouldBe` 5
            fromMaybe 3 Nothing `shouldBe` 3
        it "can convert between List and Maybe" $ do
            listToMaybe [1, 2, 3] `shouldBe` Just 1
            listToMaybe ([] :: [Integer]) `shouldBe` Nothing
            maybeToList (Just 1) `shouldBe` [1]
            maybeToList (Nothing :: Maybe Integer) `shouldBe` []
        it "can drop Nothing values from a List" $ do
            catMaybes [Just 1, Nothing, Just 2] `shouldBe` [1, 2]
            catMaybes [(Nothing :: Maybe Integer), (Nothing :: Maybe Integer)] `shouldBe` []
        it "can convert a List of Maybe into a Maybe of List" $ do
            flipMaybe [Just 1, Just 2, Just 3] `shouldBe` Just [1, 2, 3]
            flipMaybe [Just 1, Nothing, Just 3] `shouldBe` Nothing
    describe "Small library for Either" $ do
        it "can extract the left of an Either in a list" $ do
            lefts' [Left 1, Right 2, Left 3, Right 4] `shouldBe` [1, 3]
        it "can extract the right of an Either in a list" $ do
            rights' [Left 1, Right 2, Left 3, Right 4] `shouldBe` [2, 4]
        it "can partition eithers" $ do
            partitionEithers' [Left 1, Right 2, Left 3, Right 4] `shouldBe` ([1, 3], [2, 4])
        it "can convert an Either to a Maybe" $ do
            eitherMaybe' even (Right 1) `shouldBe` (Just False)
            eitherMaybe' even (Right 2) `shouldBe` (Just True)
            eitherMaybe' even (Left 2) `shouldBe` Nothing
