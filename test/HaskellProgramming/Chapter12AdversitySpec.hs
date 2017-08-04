module HaskellProgramming.Chapter12AdversitySpec where

import Test.Hspec
import HaskellProgramming.Chapter12Adversity

spec :: Spec
spec = do
    describe "String processing" $ do
        it "can replace 'the' with 'a'" $ do
            replaceThe "the cow loves us" `shouldBe` "a cow loves us"