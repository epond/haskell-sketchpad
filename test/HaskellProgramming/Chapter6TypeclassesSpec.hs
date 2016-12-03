module HaskellProgramming.Chapter6TypeclassesSpec where

import Test.Hspec
import HaskellProgramming.Chapter6Typeclasses

spec :: Spec
spec = do
    describe "Typeclasses" $ do
        it "has an Eq instance for TisAnInteger" $ do
            (TisAn 2) == (TisAn 2) `shouldBe` True
            (TisAn 2) == (TisAn 3) `shouldBe` False
