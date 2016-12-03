module HaskellProgramming.Chapter6TypeclassesSpec where

import Test.Hspec
import HaskellProgramming.Chapter6Typeclasses

spec :: Spec
spec = do
    describe "Typeclasses" $ do
        it "has an Eq instance for TisAnInteger" $ do
            TisAn 2 == TisAn 2 `shouldBe` True
            TisAn 2 == TisAn 3 `shouldBe` False
        it "has an Eq instance for TwoIntegers" $ do
            Two 7 8 == Two 7 8 `shouldBe` True
            Two 7 8 == Two 7 9 `shouldBe` False
            Two 7 8 == Two 8 8 `shouldBe` False
        it "has an Eq instance for StringOrInt" $ do
            TisAnInt 5 == TisAnInt 5 `shouldBe` True
            TisAString "abc" == TisAString "abc" `shouldBe` True
            TisAnInt 5 == TisAnInt 4 `shouldBe` False
            TisAString "abc" == TisAString "abb" `shouldBe` False
            TisAnInt 5 == TisAString "5" `shouldBe` False